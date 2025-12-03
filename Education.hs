{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DeriveGeneric       #-}

module Main where

import           Prelude                          (IO, print, putStrLn, String)
import qualified Prelude                          as H

import           PlutusTx
import           PlutusTx.Prelude                hiding (Semigroup(..), unless, ($))
import           Plutus.V2.Ledger.Api
  ( BuiltinData
  , ScriptContext (..)
  , TxInfo (..)
  , TxOut (..)
  , Validator
  , mkValidatorScript
  , PubKeyHash
  , Address (..)
  , Credential (..)
  , adaSymbol
  , adaToken
  , txOutValue
  , txOutAddress
  , txInfoOutputs
  , TokenName (..)
  , CurrencySymbol
  , txInfoMint
  , POSIXTime
  )
import           Plutus.V2.Ledger.Contexts      (txSignedBy, ownCurrencySymbol)
import qualified Plutus.V1.Ledger.Value         as Value

import qualified Data.ByteString.Lazy            as LBS
import qualified Data.ByteString.Short           as SBS
import           Codec.Serialise                 (serialise)

import           Cardano.Api                     (writeFileTextEnvelope)
import           Cardano.Api.Shelley             (PlutusScript (..), PlutusScriptV2)

--------------------------------------------------------------------------------
-- Datum & Redeemer types (education)
--------------------------------------------------------------------------------

-- Scholarship recipient (pubkey + amount in lovelace)
data Scholarship = Scholarship
    { schPkh  :: PubKeyHash
    , schAmt  :: Integer
    }
PlutusTx.unstableMakeIsData ''Scholarship

-- Milestone for tuition (index, amount, optional release time)
data Milestone = Milestone
    { mIndex :: Integer
    , mAmount :: Integer
    , mDueAt  :: POSIXTime
    }
PlutusTx.unstableMakeIsData ''Milestone

-- Main datum for the education validator
data EducationEscrow = EducationEscrow
    { payer         :: PubKeyHash       -- who deposited tuition (guardian/sponsor)
    , student       :: PubKeyHash
    , school        :: PubKeyHash       -- authorized school to sign releases
    , totalAmount   :: Integer          -- total lovelace locked
    , milestones    :: [Milestone]      -- per-term/class release schedule
    , scholarships  :: [Scholarship]    -- optional scholarship payments per release
    , claimed       :: [Integer]        -- indices of milestones already claimed
    , certIssuer    :: PubKeyHash       -- who can issue certificate NFT
    , certTokenName :: BuiltinByteString -- expected token name for certificate NFT
    , certIssued    :: Bool             -- whether certificate already issued (simple guard)
    }
PlutusTx.unstableMakeIsData ''EducationEscrow

-- Actions the validator accepts
data Action = Lock | Release Integer | Refund | IssueCert
PlutusTx.unstableMakeIsData ''Action

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

{-# INLINABLE pubKeyHashAddress #-}
pubKeyHashAddress :: PubKeyHash -> Address
pubKeyHashAddress pkh = Address (PubKeyCredential pkh) Nothing

{-# INLINABLE valuePaidTo #-}
-- Sum of ADA paid to a given pubkey in tx outputs
valuePaidTo :: TxInfo -> PubKeyHash -> Integer
valuePaidTo info pkh =
    let outs = txInfoOutputs info
        matches = [ Value.valueOf (txOutValue o) adaSymbol adaToken
                  | o <- outs
                  , txOutAddress o == pubKeyHashAddress pkh
                  ]
    in foldr (+) 0 matches

{-# INLINABLE milestoneByIndex #-}
milestoneByIndex :: [Milestone] -> Integer -> Maybe Milestone
milestoneByIndex ms idx = case filter (\m -> mIndex m == idx) ms of
                            [] -> Nothing
                            (x:_) -> Just x

{-# INLINABLE isNotClaimed #-}
isNotClaimed :: [Integer] -> Integer -> Bool
isNotClaimed claimed idx = notElem idx claimed

{-# INLINABLE sumScholarships #-}
sumScholarships :: [Scholarship] -> Integer
sumScholarships schs = foldr (\s acc -> acc + schAmt s) 0 schs

{-# INLINABLE scholarshipsPaidOk #-}
scholarshipsPaidOk :: TxInfo -> [Scholarship] -> Bool
scholarshipsPaidOk info schs =
    foldr (\s acc -> acc && (valuePaidTo info (schPkh s) >= schAmt s)) True schs

--------------------------------------------------------------------------------
-- Core validator
--------------------------------------------------------------------------------

{-# INLINABLE mkEducationValidator #-}
mkEducationValidator :: EducationEscrow -> Action -> ScriptContext -> Bool
mkEducationValidator datum action ctx =
    case action of
      Lock ->
        -- allow locking without extra checks (off-chain should ensure amount matches datum)
        True

      Refund ->
        -- only the original payer can request refund (off-chain should rebuild UTXO)
        traceIfFalse "refund: payer signature required" (txSignedBy info (payer datum))

      Release idx ->
        case milestoneByIndex (milestones datum) idx of
          Nothing -> traceError "release: milestone not found"
          Just m  ->
            traceIfFalse "release: milestone already claimed" (isNotClaimed (claimed datum) idx)
            && traceIfFalse "release: school must sign" (txSignedBy info (school datum))
            && traceIfFalse "release: insufficient payment to school" (paidToSchoolOK info datum m)
            && traceIfFalse "release: scholarships not paid" (scholarshipsPaidOk info (scholarships datum))

      IssueCert ->
        -- certificate issuance: issuer must sign, cert not already issued,
        -- tx must mint exactly 1 of the expected TokenName under this script's currency symbol,
        -- and the minted token must be paid to the student's pubkey address
        traceIfFalse "issuecert: cert issuer must sign" (txSignedBy info (certIssuer datum))
        && traceIfFalse "issuecert: cert already issued" (not (certIssued datum))
        && traceIfFalse "issuecert: minted exactly one expected token" (mintedExactlyOne info ctx (certTokenName datum))
        && traceIfFalse "issuecert: minted token paid to student" (mintPaidToStudent info ctx (student datum) (certTokenName datum))
  where
    info = scriptContextTxInfo ctx

{-# INLINABLE paidToSchoolOK #-}
paidToSchoolOK :: TxInfo -> EducationEscrow -> Milestone -> Bool
paidToSchoolOK info datum m =
    let amount = mAmount m
        scholarshipTotal = sumScholarships (scholarships datum)
        expectedToSchool = amount - scholarshipTotal
        paidToSchool = valuePaidTo info (school datum)
    in expectedToSchool >= 0 && paidToSchool >= expectedToSchool

{-# INLINABLE mintedExactlyOne #-}
-- Check txInfoMint contains exactly 1 of token with given name under this script's currency symbol
mintedExactlyOne :: TxInfo -> ScriptContext -> BuiltinByteString -> Bool
mintedExactlyOne info ctx tnBs =
    let cs = ownCurrencySymbol ctx
        tn = TokenName tnBs
        minted = txInfoMint info
    in Value.valueOf minted cs tn == 1

{-# INLINABLE mintPaidToStudent #-}
-- Ensure one of the outputs to student's address contains the minted token
mintPaidToStudent :: TxInfo -> ScriptContext -> PubKeyHash -> BuiltinByteString -> Bool
mintPaidToStudent info ctx pkh tnBs =
    let cs = ownCurrencySymbol ctx
        tn = TokenName tnBs
        outs = txInfoOutputs info
        matches = [ True
                  | o <- outs
                  , txOutAddress o == pubKeyHashAddress pkh
                  , Value.valueOf (txOutValue o) cs tn >= 1
                  ]
    in case matches of
         [] -> False
         (_:_) -> True

--------------------------------------------------------------------------------
-- Wrap & compile
--------------------------------------------------------------------------------

{-# INLINABLE wrapped #-}
wrapped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrapped d r c =
    let esc = unsafeFromBuiltinData d :: EducationEscrow
        act = unsafeFromBuiltinData r :: Action
        ctx = unsafeFromBuiltinData c :: ScriptContext
    in if mkEducationValidator esc act ctx
         then ()
         else traceError "EducationEscrow: validation failed"

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| wrapped ||])

--------------------------------------------------------------------------------
-- Write validator to file
--------------------------------------------------------------------------------

saveValidator :: IO ()
saveValidator = do
    let scriptSerialised = serialise validator
        scriptShortBs    = SBS.toShort (LBS.toStrict scriptSerialised)
        plutusScript     = PlutusScriptSerialised scriptShortBs :: PlutusScript PlutusScriptV2
    r <- writeFileTextEnvelope "education-escrow-validator.plutus" Nothing plutusScript
    case r of
      Left err -> print err
      Right () -> putStrLn "SEducation escrow validator written to: education-escrow-validator.plutus"

main :: IO ()
main = saveValidator
