

# 🎓 **Education Escrow Smart Contract – Full Tutorial**

A complete walkthrough of how your *milestone-based tuition payment* + *scholarship* + *certificate NFT issuance* validator works.

---

# 📚 **Table of Contents**

1. [🎯 Purpose of the Contract](#1-purpose-of-the-contract)
2. [📄 Datum Types Explained](#2-datum-types-explained)
3. [🔄 Redeemer Actions](#3-redeemer-actions)
4. [🛠️ Helper Functions](#4-helper-functions)
5. [🧠 Core Validator Logic](#5-core-validator-logic)
6. [🏫 Milestone Release Logic](#6-milestone-release-logic)
7. [🎓 Certificate NFT Minting Logic](#7-certificate-nft-minting-logic)
8. [💰 Scholarship Settlement Logic](#8-scholarship-settlement-logic)
9. [⚙️ Script Compilation](#9-script-compilation)
10. [🚀 Off-chain Workflow (Recommended)](#10-off-chain-workflow-recommended)
11. [🧪 Testing Scenarios](#11-testing-scenarios)

---

# 1. 🎯 Purpose of the Contract

This validator implements a complete **education payment system**:

✔ Tuition is locked on-chain by a guardian/sponsor
✔ Funds release only when **school signs**
✔ Releases follow **milestones** (classes, terms, semesters)
✔ Scholarships automatically split out a portion of each milestone
✔ Guardian may request refund
✔ At completion, school issues a **Certificate NFT** to the student

This creates a **trustless educational funding model** with:

✓ Transparent milestone-based spending
✓ Integrated scholarship disbursement
✓ On-chain certificate proof
✓ Multi-party approval (payer + school + student)

---

# 2. 📄 Datum Types Explained

Your `EducationEscrow` datum captures *all state* of the escrow.

### 📌 **Scholarship**

```haskell
data Scholarship = Scholarship
    { schPkh  :: PubKeyHash
    , schAmt  :: Integer
    }
```

Represents fixed scholarship payments to recipients each time a milestone is released.

---

### 📌 **Milestone**

```haskell
data Milestone = Milestone
    { mIndex :: Integer
    , mAmount :: Integer
    , mDueAt  :: POSIXTime
    }
```

Each milestone includes:

| Field  | Meaning                                    |
| ------ | ------------------------------------------ |
| index  | Milestone number                           |
| amount | Amount school should receive               |
| dueAt  | POSIX deadline (not enforced on-chain yet) |

---

### 📌 **EducationEscrow Datum Structure**

The full datum includes:

```haskell
data EducationEscrow = EducationEscrow
    { payer         :: PubKeyHash
    , student       :: PubKeyHash
    , school        :: PubKeyHash
    , totalAmount   :: Integer
    , milestones    :: [Milestone]
    , scholarships  :: [Scholarship]
    , claimed       :: [Integer]
    , certIssuer    :: PubKeyHash
    , certTokenName :: BuiltinByteString
    , certIssued    :: Bool
    }
```

### Key elements:

✔ **payer** — guardian funding the contract
✔ **school** — must sign releases
✔ **student** — recipient of final certificate NFT
✔ **milestones** — release schedule
✔ **scholarships** — extra payouts for each release
✔ **claimed** — prevents milestone double spending
✔ **certificate minting** — includes authority + token name

---

# 3. 🔄 Redeemer Actions

The validator accepts 4 actions:

```haskell
data Action = Lock | Release Integer | Refund | IssueCert
```

| Action        | Purpose                                        |
| ------------- | ---------------------------------------------- |
| **Lock**      | Deposit funds into escrow                      |
| **Release i** | Release milestone *i* to school + scholarships |
| **Refund**    | Payer withdraws remaining funds                |
| **IssueCert** | School mints certificate NFT to student        |

---

# 4. 🛠️ Helper Functions

### 🧭 `pubKeyHashAddress`

Builds an address from a public key hash.

---

### 💸 `valuePaidTo`

Counts the ADA sent to a specific address in the tx outputs.

Essential for verifying:

* payments to school
* scholarship payouts

---

### 🔍 `milestoneByIndex`

Retrieves a milestone by its index.

---

### 🚫 `isNotClaimed`

Prevents double-claiming of milestones.

---

### 🎓 `scholarshipsPaidOk`

Verifies scholarships are fully paid.

---

# 5. 🧠 Core Validator Logic

The main validator is:

```haskell
mkEducationValidator :: EducationEscrow -> Action -> ScriptContext -> Bool
```

Each action branches into its own validation rules.

---

## **A. Lock**

```haskell
Lock -> True
```

No validation needed — off-chain ensures amounts match.

---

## **B. Refund**

```haskell
Refund ->
  txSignedBy info (payer datum)
```

Only the payer can withdraw unused tuition.

---

## **C. Release idx**

This is the heart of the contract.

### Release requires:

✔ **Valid milestone exists**
✔ **Not claimed before**
✔ **School signature**
✔ **School receives m.amount – scholarshipTotal**
✔ **All scholarships funded**

---

## **D. IssueCert**

Minting certificate NFT:

✔ **certIssuer signs**
✔ Not issued before
✔ Mint exactly **1** NFT with correct TokenName
✔ NFT is sent to **student address**

---

# 6. 🏫 Milestone Release Logic

Milestones follow this enforcement:

```haskell
traceIfFalse "release: milestone already claimed"
(isNotClaimed ...)
```

Payment to school:

```haskell
paidToSchool >= (mAmount - scholarshipTotal)
```

Scholarships:

```haskell
scholarshipsPaidOk info (scholarships datum)
```

School approval:

```haskell
txSignedBy info (school datum)
```

This ensures:

✓ No double claims
✓ Tuition + scholarships must sum up exactly
✓ School must authorize every class/term/semester release

---

# 7. 🎓 Certificate NFT Minting Logic

Certificate minting is tightly controlled:

### Validator checks:

1. **Issuer signature**
2. **NFT not already issued**
3. **Exactly one token minted**
4. **Token goes to student**

This creates:

✔ Graduate-verifiable proof
✔ Anti-forgery minting
✔ Single certificate limit
✔ On-chain permanent record

---

# 8. 💰 Scholarship Settlement Logic

Every milestone triggers *automatic scholarship payouts*:

```haskell
scholarshipsPaidOk info schs
```

Each `Scholarship` entry must receive at least `schAmt` lovelace.

This enables:

✓ Tuition split funding
✓ Combined sponsorship
✓ Conditional scholarship disbursements

---

# 9. ⚙️ Script Compilation

Your validator is compiled and written to:

```
education-escrow-validator.plutus
```

Using:

```haskell
saveValidator
```

This produces a Plutus V2 script ready for:

* `cardano-cli`
* Mesh / Lucid / Helios DApps
* Backends

---

# 10. 🚀 Off-chain Workflow (Recommended)

### **1. Lock**

Guardian deposits ADA with datum.

### **2. Release milestone i**

School signs → school + scholarship recipients get paid.

### **3. Refund**

Unused funds returned to payer.

### **4. IssueCert**

School issues NFT to student.

---

# 11. 🧪 Testing Scenarios

### ✔ Successful milestone release

✓ school signs
✓ correct payments
✓ scholarships funded

### ❌ Wrong school signature

Should fail.

### ❌ Milestone already claimed

Should fail.

### ✔ Successful certificate mint

✓ issuer signs
✓ minted 1 NFT
✓ NFT to student

### ❌ Attempt second certificate

Should fail (certIssued = True).

---

