Plutus Pioneer Program - Cohort 3 
January 26, 2022

Contributed By:
[Joe Totes](https://github.com/Totes5706)

Offical Video by Lars Brünjes: [PPP-Cohort3-Lecture3](https://youtu.be/sLMhsqiWeGU)

Google Doc version can be found [HERE](https://docs.google.com/document/d/1MKEcgNl5QUugBhan39eOKM6zkBJ9_kLkQ6abjWp9pUY/edit#)

# Lecture 3 : Script Context and the Cardano Testnet

## Table of Contents
- [Lecture 3 : Script Context and the Cardano Testnet](#lecture-3-script-context-and-the-cardano-testnet)
  - [Table of Contents](#table-of-contents)
  - [Preparation for Lecture 3](#preperation-for-lecture-3)
  - [Plutus Playground Timeout](#plutus-playground-timeout)
  - [Script Context](#script-context)
  - [Handling Time](#handling-time)
  - [Vesting Contract](#vesting-contract)
  - [Parameterized Contract](#parameterized-contract)
  - [Cardano Testnet](#cardano-testnet)
  - [Homework Part 1](#homework-part-1)
  - [Homework Part 2](#homework-part-2)


## Preparation for Lecture 3

Before we can get started in Lecture 3, we first must get our development environment up to date. You can copy and paste any of the code in this guide directly into your terminal or IDE.

First, head to the plutus-pioneer-program directory to grab the lecture week 3 contents. Execute: 

```
totinj@penguin:~/plutus-pioneer-program$ git pull
```

You can now navigate to the current week03 directory and open the cabal.project file:

```
totinj@penguin:~/plutus-pioneer-program/code/week03$ cat cabal.project
```

 Grab the plutus-apps tag inside the cabal.project file:
 
```
location: https://github.com/input-output-hk/plutus-apps.git
  tag:4edc082309c882736e9dec0132a3c936fe63b4ea
```

Head back to  to the plutus-apps directory and update it to the  current git tag:

```
totinj@penguin:~/plutus-apps$ git checkout main
```
```
totinj@penguin:~/plutus-apps$ git pull
```
```
totinj@penguin:~/plutus-apps$ git checkout 4edc082309c882736e9dec0132a3c936fe63b4ea
```

You should now be up to date and can run nix-shell in this directory. Run nix-shell:

```
totinj@penguin:~/plutus-apps$ nix-shell
```

Head back to the week03 folder to start running the cabal commands:

```
[nix-shell:~/plutus-pioneer-program/code/week03]$ cabal update
```
```
[nix-shell:~/plutus-pioneer-program/code/week03]$ cabal build
```
```
[nix-shell:~/plutus-pioneer-program/code/week03]$ cabal repl
```

If successful,  you should now see in the terminal:

```haskell
Ok, 7 modules loaded.
Prelude week03.Deploy> 
```

This lecture will also explore the Cardano Testnet. In order to interact with it later, we need to sync the node first locally which may take 5+ hours. Let’s get it started in the background:

Keep the cabal repl open on terminal 1, and open a new terminal 2. Head to the plutus-apps directory and first run nix-shell:

```
Terminal 2
totinj@penguin:~/plutus-apps$ nix-shell
```

We can check the version of the Cardano Node and with the commands:

```
Terminal 2
[nix-shell:~/plutus-apps]$ cardano-node --version

Output:
cardano-node 1.33.0 - linux-x86_64 - ghc-8.10
git rev 0000000000000000000000000000000000000000
```

```
Terminal 2
[nix-shell:~/plutus-apps]$ cardano-cli --version

Output:
cardano-cli 1.33.0 - linux-x86_64 - ghc-8.10
git rev 0000000000000000000000000000000000000000
```


Head to week03 subfolder in the plutus pioneer directory, then inside that the testnet folder. We will be running the start-node-test.sh script which will initialize download the testnet blockchain based off of the config files in this folder:

```
Terminal 2
[nix-shell:~/plutus-pioneer-program/code/week03/testnet]$ 
./start-node-testnet.sh

Output:
[penguin:cardano.node.ChainDB:Notice:34] [2022-02-22 14:23:14.33 UTC] Chain extended, new tip: 73912d4f092b24fcbb0b9f8f7e4668026ca91d6dba2b2758b62b704766e1faa7 at slot 51170578
```

Where start-node-test.sh looks like:

```haskell
cardano-node run \
 --topology testnet-topology.json \
 --database-path db \
 --socket-path node.socket \
 --host-addr 127.0.0.1 \
 --port 3001 \
 --config testnet-config.json
```

This process will take 5+ hours to sync. You will be 100% synced once you start seeing a new block every 20 seconds, rather than multiple blocks per second. Leave this terminal open and we can now get started.



## Plutus Playground Timeout


If the plutus playground server experiences a timeout before completing, we can use this command instead to extend the runtime:

```
[nix-shell:~/plutus-apps/plutus-playground-server]$ 
plutus-playground-server -i 120s
```
## Script Context



In this lecture, we will be exploring the script context. If we remember for lecture 2, script context is the third piece of on-chain data that defines the purpose for running:

```haskell
data ScriptContext
Constructors
ScriptContext
 
scriptContextTxInfo :: TxInfo
scriptContextPurpose ::ScriptPurpose
```

```haskell
data ScriptPurpose
Purpose of the script that is currently running
Constructors
Minting CurrencySymbol
 
Spending TxOutRef
 
Rewarding StakingCredential
 
Certifying DCert
```
```haskell
data TxInfo
A pending transaction. This is the view as seen by validator scripts, so some details are stripped out.
Constructors
TxInfo
 
txInfoInputs :: [TxInInfo]
Transaction inputs
txInfoOutputs :: [TxOut]
Transaction outputs
txInfoFee :: Value
The fee paid by this transaction.
txInfoMint :: Value
The Value minted by this transaction.
txInfoDCert :: [DCert]
Digests of certificates included in this transaction
txInfoWdrl :: [(StakingCredential, Integer)]
Withdrawals
txInfoValidRange :: POSIXTimeRange
The valid range for the transaction.
txInfoSignatories :: [PubKeyHash] 
Signatures provided with the transaction, attested that they all signed the tx
txInfoData :: [(DatumHash, Datum)]
txInfoId :: TxId  Hash of the pending transaction (excluding witnesses)
```

If we look at cardano docs, we can see a simple example:

Intuitive example
For example, a kid wants to go on a Ferris wheel, but before getting on, they must be taller than the safety sign.
We could express that idea in pseudo code, like:

```haskell
if isTallEnough(attraction=ferrisWheel,passenger=michael):
   getOnFerrisWheel()


def isTallEnough(attraction,kid):
   return kid["height"] >= attraction["minimumHeight"]


def getOnFerrisWheel():
   print ("get On the Ferris Wheel")


ferrisWheel = {"minimumHeight":120}
michael = {"height":135}
```

In this example the following applies:

 - The datum is the information about this transaction: ```michael.height```.
 - The context is the state of the world, at that point meaning: ```ferrisWheel.minimumHeight```.
 - The reedemer, is the action to perform: ```getOnFerrisWheel()```
 - The validator script is the function that uses all that information ```isTallEnough```

## Handling Time

In the Cardano eUTxO model, transactions can still fail. This is because a transaction can consume an input, that when that transaction arrives on the blockchain at the node for validation,it could have already been consumed by another person. But in that case, the transaction simply fails without having to pay fees. But what can never have or should never happen under normal circumstances, is that a validation script runs and then fails. The failure should happen before it is even submitted. It is a great feature, however it has many implications about how to express time.

We want to be able to express validation logic that says that a certain transaction is only valid after a certain time has been reached or before a certain time has been reached. We saw an example of that in the very first example, the auction example, the bids are only allowed until the deadline has been reached. The close endpoint can only be called after the deadline has passed. 

If you think about that, that seems to be a contradiction because time is obviously flowing. When you try to validate a transaction that you're constructing in your wallet, the time that happens in the wallet can of course be different from the time that the transaction arrives at a node for validation. It is not clear how to bring these two together to on the one hand handle time, but on the other hand guarantee that validation is deterministic in the sense that if  it succeeds in the wallet, it will also succeed in the node.

Cardano solves this, by adding this POSIX time range field and TX info valid range field to a transaction. With this, we can declare a transaction is valid between a specified time range specified in the transaction. When a node is validating a transaction, one of these pre-checks before validation, is the node checks the current time and compares it to the time range specified in the transaction. If the current time does not fall into this time range, then validation fails immediately without ever running the validator scripts. That also means that if these pre-checks succeed, then we can assume that the current time does fall into this interval. This preserves the deterministic eUTxO properties.

By default, all transactions use the infinite time range. This starts at the beginning of time or at the Genesis block,
and lasts for all eternity. These transactions will always be valid, no matter at what time they arrive at a node for validation.
The only exceptions we have seen so far were those in the auction example, where the bid and the close couldn't use the infinite interval because we made sure that the bid happens before the deadline and the close after the deadline. However by default, all transactions including those that you send from Daedalus for example, will always use the infinite time range.

There is one slight complication that Ouroboros, the consensus protocol powering Cardano, does not use POSIX time; it uses slots. Plutus uses real time, so we need to be able to convert back and forth between real time and slots. Right now, the slot length is one second. Knowing that, it is easy to go back and forth between real time and slot numbers. However, this could change in future through a parameter change via a hard fork.  And, of course, we can't know that in advance.

We do not know right now what the slot length will be in 10 years, for example. This means that we must not have a definite upper bound. We know what the slot length will be in the next 36 hours because if there's a change in protocol parameters, then we know that at least 36 hours in advance.You can not specify arbitrary time ranges in the transaction interval. It must only be at most 36 hours in the future, or it can be indefinite.

So let's look at this POSIX time range type.

```haskell
type POSIXTimeRange = Interval POSIXTime
An Interval of POSIXTimes.
```

Where Interval is:

```haskell
data Interval a
An interval of as.
The interval may be either closed or open at either end, meaning that the endpoints may or may not be included in the interval.
The interval can also be unbounded on either side.
Constructors
Interval
 
ivFrom :: LowerBound a 
ivTo :: UpperBound a
```

Where Lower Bound is:

```haskell
data LowerBound a
The lower bound of an interval.
Constructors
LowerBound (Extended a) Closure
```

Where Closure is:

```haskell
type Closure = Bool
Whether a bound is inclusive or not.
```

Where Extended is:

```haskell
data Extended a
A set extended with a positive and negative infinity.
Constructors
NegInf
 
Finite a
 
PosInf
``` 

Some useful functions for defining bounds:

```haskell
after :: Ord a => a -> Interval a -> Bool
Check if a value is later than the end of a Interval.
```

```haskell
before :: Ord a => a -> Interval a -> Bool
Check if a value is earlier than the beginning of an Interval.
```

```haskell
isEmpty :: (Enum a, Ord a) => Interval a -> Bool
Check if an Interval is empty.
 ```
 
 ```haskell
contains :: Ord a => Interval a -> Interval a -> Bool
a contains b is true if the Interval b is entirely contained in a. That is, a contains b if for every entry s, if member s b then member s a.
```

```haskell
hull :: Ord a => Interval a -> Interval a -> Interval a
'hull a b' is the smallest interval containing a and b.
```

```haskell
intersection :: Ord a => Interval a -> Interval a -> Interval a
'intersection a b' is the largest interval that is contained in a and in b, if it exists.
```

```haskell
overlaps :: (Enum a, Ord a) => Interval a -> Interval a -> Bool
Check whether two intervals overlap, that is, whether there is a value that is a member of both intervals.
```

```haskell
member :: Ord a => a -> Interval a -> Bool
Check whether a value is in an interval.
```

```haskell
never :: Interval a
An Interval that is empty.
```

```haskell
always :: Interval a
An Interval that covers every slot.
```

```haskell
to :: a -> Interval a
to a is an Interval that includes all values that are smaller than or equal to a.
```

```haskell
from :: a -> Interval a
from a is an Interval that includes all values that are greater than or equal to a.
```

```haskell
singleton :: a -> Interval a
```

```haskell
interval :: a -> a -> Interval a
interval a b includes all values that are greater than or equal to a and smaller than or equal to b. Therefore it includes a and b.
```

```haskell
upperBound :: a -> UpperBound a
```

```haskell
lowerBound :: a -> LowerBound a
```

```haskell
strictLowerBound :: a -> LowerBound a
```

```haskell
strictUpperBound :: a -> UpperBound a
```

We can now get some practice in the cabal repl. We will first import Plutus.V1.Ledger.Interval.

```
Prelude week03.Deploy> import Plutus.V1.Ledger.Interval
```


Example Interval:


Prelude Plutus.V1.Ledger.Interval week03.Deploy> 
interval (10 :: Integer) 20

Output:
Interval {ivFrom = LowerBound (Finite 10) True, ivTo = UpperBound (Finite 20) True}



Example Member:


Prelude Plutus.V1.Ledger.Interval week03.Deploy> 
member 9 $ interval (10 :: Integer) 20

Output:
False




Prelude Plutus.V1.Ledger.Interval week03.Deploy> 
member 10 $ interval (10 :: Integer) 20

Output:
True




Prelude Plutus.V1.Ledger.Interval week03.Deploy> 
member 21 $ interval (10 :: Integer) 20

Output:
False



Example From:


Prelude Plutus.V1.Ledger.Interval week03.Deploy> 
member 10 $ from (30 :: Integer)

Output:
False



Prelude Plutus.V1.Ledger.Interval week03.Deploy> 
member 30 $ from (30 :: Integer)

Output:
True




Prelude Plutus.V1.Ledger.Interval week03.Deploy> 
member 31 $ from (30 :: Integer)

Output:
True



Example to:


Prelude Plutus.V1.Ledger.Interval week03.Deploy> 
member 10 $ to (30 :: Integer)

Output:
True



Prelude Plutus.V1.Ledger.Interval week03.Deploy> 
member 30 $ to (30 :: Integer)

Output:
True


Prelude Plutus.V1.Ledger.Interval week03.Deploy> 
member 31 $ to (30 :: Integer)

Output:
False








Example Intersection:



Prelude Plutus.V1.Ledger.Interval week03.Deploy> 
intersection (interval (10 :: Integer) 20) $ interval 18 30

Output:
Interval {ivFrom = LowerBound (Finite 18) True, ivTo = UpperBound (Finite 20) True}



Example Contains:


Prelude Plutus.V1.Ledger.Interval week03.Deploy> 
contains (to (100 :: Integer)) $ interval 30 80

Output:
True



Prelude Plutus.V1.Ledger.Interval week03.Deploy> 
contains (to (100 :: Integer)) $ interval 30 100

Output:
True



Prelude Plutus.V1.Ledger.Interval week03.Deploy> 
contains (to (100 :: Integer)) $ interval 30 101

Output:
False

Example Overlaps:



Prelude Plutus.V1.Ledger.Interval week03.Deploy> 
overlaps (to (100 :: Integer)) $ interval 30 101

Output:
True




Prelude Plutus.V1.Ledger.Interval week03.Deploy> 
overlaps (to (100 :: Integer)) $ interval 101 110

Output:
False





















Vesting Contract




 We will now look at an example vesting contract. Imagine you want to make a gift of ADA to a child. You want the child to own the ADA, however you only want the child to have access to the ADA when he or she gets to a specified age. Using plutus, it is very easy to implement a vesting scheme that satisfies those conditions.

We first look at the datum being passed with two pieces of information; the beneficiary and the deadline:
data VestingDatum = VestingDatum
   { beneficiary :: PaymentPubKeyHash
   , deadline    :: POSIXTime
   } deriving Show


We then look at the validator function:

{-# INLINABLE mkValidator #-}
mkValidator :: VestingDatum -> () -> ScriptContext -> Bool
mkValidator dat () ctx = traceIfFalse "beneficiary's signature missing" signedByBeneficiary &&
                        traceIfFalse "deadline not reached" deadlineReached
 where
   info :: TxInfo
   info = scriptContextTxInfo ctx

   signedByBeneficiary :: Bool
   signedByBeneficiary = txSignedBy info $ unPaymentPubKeyHash $ beneficiary dat

   deadlineReached :: Bool
   deadlineReached = contains (from $ deadline dat) $ txInfoValidRange info


We defined the datum as dat and context as ctx. We then check the correct beneficiary by creating the signedByBeneficiary function and the deadline by the deadlineReached function.


Then encode the datum and redeemer:

data Vesting
instance Scripts.ValidatorTypes Vesting where
   type instance DatumType Vesting = VestingDatum
   type instance RedeemerType Vesting = ()



Now we need to handle the compilation:

typedValidator :: Scripts.TypedValidator Vesting
typedValidator = Scripts.mkTypedValidator @Vesting
   $$(PlutusTx.compile [|| mkValidator ||])
   $$(PlutusTx.compile [|| wrap ||])
 where
   wrap = Scripts.wrapValidator @VestingDatum @()





Followed by the boilerplate code for the validator, hash, and address:

validator :: Validator
validator = Scripts.validatorScript typedValidator

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash typedValidator

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator






Lastly, the off-chain code:

data GiveParams = GiveParams
   { gpBeneficiary :: !PaymentPubKeyHash
   , gpDeadline    :: !POSIXTime
   , gpAmount      :: !Integer
   } deriving (Generic, ToJSON, FromJSON, ToSchema)

type VestingSchema =
           Endpoint "give" GiveParams
       .\/ Endpoint "grab" ()

give :: AsContractError e => GiveParams -> Contract w s e ()
give gp = do
   let dat = VestingDatum
               { beneficiary = gpBeneficiary gp
               , deadline    = gpDeadline gp
               }
       tx  = Constraints.mustPayToTheScript dat $ Ada.lovelaceValueOf $ gpAmount gp
   ledgerTx <- submitTxConstraints typedValidator tx
   void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
   logInfo @String $ printf "made a gift of %d lovelace to %s with deadline %s"
       (gpAmount gp)
       (show $ gpBeneficiary gp)
       (show $ gpDeadline gp)

grab :: forall w s e. AsContractError e => Contract w s e ()
grab = do
   now   <- currentTime
   pkh   <- ownPaymentPubKeyHash
   utxos <- Map.filter (isSuitable pkh now) <$> utxosAt scrAddress
   if Map.null utxos
       then logInfo @String $ "no gifts available"
       else do
           let orefs   = fst <$> Map.toList utxos
               lookups = Constraints.unspentOutputs utxos  <>
                         Constraints.otherScript validator
               tx :: TxConstraints Void Void
               tx      = mconcat [Constraints.mustSpendScriptOutput oref unitRedeemer | oref <- orefs] <>
                         Constraints.mustValidateIn (from now)
           ledgerTx <- submitTxConstraintsWith @Void lookups tx
           void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
           logInfo @String $ "collected gifts"
 where
   isSuitable :: PaymentPubKeyHash -> POSIXTime -> ChainIndexTxOut -> Bool
   isSuitable pkh now o = case _ciTxOutDatum o of
       Left _          -> False
       Right (Datum e) -> case PlutusTx.fromBuiltinData e of
           Nothing -> False
           Just d  -> beneficiary d == pkh && deadline d <= now

endpoints :: Contract () VestingSchema Text ()
endpoints = awaitPromise (give' `select` grab') >> endpoints
 where
   give' = endpoint @"give" give
   grab' = endpoint @"grab" $ const grab

mkSchemaDefinitions ''VestingSchema

mkKnownCurrencies []



We can now test this in Plutus Playground.

In order to get started with Plutus Playground, we need to have two terminals running, both of which are in the nix-shell.

Let’s get started with terminal 1. Head to the plutus-apps directory and first run nix-shell:



Terminal 1
totinj@penguin:~/plutus-apps$ nix-shell



Next we head to plutus-playground-server directory and run: 

Terminal 1
[nix-shell:~/plutus-apps/plutus-playground-server]$ plutus-playground-server




If Successful, you will see the output:

Terminal 1
Interpreter Ready



Let’s get started with terminal 2. Head to the plutus-apps directory and first run nix-shell:



Terminal 2
totinj@penguin:~/plutus-apps$ nix-shell



Next we head to plutus-playground-client directory and run: 

Terminal 2
[nix-shell:~/plutus-apps/plutus-playground-client]$ npm run start


If Successful, you will see the output:

Terminal 2
[wdm]: Compiled successfully.

or

[wdm]: Compiled with warnings.



Keep both terminals open, and we should now be able to access Plutus Playground from the browser.



Open a browser and head to the address:

https://localhost:8009


You will get a warning complaining about it being a risky website, ignore the message to click through anyway.

You should now be able to successfully compile and run the gift contract by copy/pasting it into Plutus Playground and using the two buttons in the top right corner: “Compile” and “Simulate”
Our wallet setup should look like:

Before we do our simulation, we need to find out the paymentpubkeyhash for wallets 2 and 3. We can do this in the repl:


Prelude week03.Deploy> import Wallet.Emulator



Prelude Wallet.Emulator week03.Deploy> 
mockWalletPaymentPubKeyHash $ knownWallet 2

Output:
80a4f45b56b88d1139da23bc4c3c75ec6d32943c087f250b86193ca7



Prelude Wallet.Emulator week03.Deploy> 
mockWalletPaymentPubKeyHash $ knownWallet 3

Output:
2e0ad60c3207248cecd47dbde3d752e0aad141d6b8f81ac2c6eca27c




We can copy/paste those hashes into the sim for wallets 2 and 3.
We also need to convert the slots to POSIXTime, which we can also do in the repl:


Prelude week03.Deploy> import Ledger.TimeSlot



Prelude Ledger.TimeSlot week03.Deploy> import Data.Default



Prelude Ledger.TimeSlot Data.Default week03.Deploy>
slotToBeginPOSIXTime def 10

Output:
POSIXTime {getPOSIXTime = 1596059101000}



Prelude Ledger.TimeSlot Data.Default week03.Deploy>
slotToBeginPOSIXTime def 20

Output:
POSIXTime {getPOSIXTime = 1596059111000}















 The wallets should look like:

Genesis Slot 0 looks like:





Slot 1, TX 0:






Slot 2, TX 0:






Slot 3, TX 0:





Slot 12, TX 0:









Slot 12, TX 1:








Final Balances:
















Parameterized Contract




We will now look at a similar example of the vesting contract, except we will be passing a parameter instead of a datum. We can first look at mkValidator, where the datum is now typ unit ():

mkValidator :: VestingParam -> () -> () -> ScriptContext -> Bool
mkValidator p () () ctx = traceIfFalse "beneficiary's signature missing" signedByBeneficiary &&
                         traceIfFalse "deadline not reached" deadlineReached
 where
   info :: TxInfo
   info = scriptContextTxInfo ctx

   signedByBeneficiary :: Bool
   signedByBeneficiary = txSignedBy info $ unPaymentPubKeyHash $ beneficiary p

   deadlineReached :: Bool
   deadlineReached = contains (from $ deadline p) $ txInfoValidRange info


Then encode the datum to type unit:

data Vesting
instance Scripts.ValidatorTypes Vesting where
   type instance DatumType Vesting = ()
   type instance RedeemerType Vesting = ()


Modifying the compilation:
typedValidator :: VestingParam -> Scripts.TypedValidator Vesting
typedValidator p = Scripts.mkTypedValidator @Vesting
   ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode p)
   $$(PlutusTx.compile [|| wrap ||])
 where
   wrap = Scripts.wrapValidator @() @()

Followed by the boilerplate code for the validator, hash, and address:

validator :: VestingParam -> Validator
validator = Scripts.validatorScript . typedValidator

valHash :: VestingParam -> Ledger.ValidatorHash
valHash = Scripts.validatorHash . typedValidator

scrAddress :: VestingParam -> Ledger.Address
scrAddress = scriptAddress . validator


Followed by the off-chain code:
data GiveParams = GiveParams
   { gpBeneficiary :: !PaymentPubKeyHash
   , gpDeadline    :: !POSIXTime
   , gpAmount      :: !Integer
   } deriving (Generic, ToJSON, FromJSON, ToSchema)

type VestingSchema =
           Endpoint "give" GiveParams
       .\/ Endpoint "grab" POSIXTime

give :: AsContractError e => GiveParams -> Contract w s e ()
give gp = do
   let p  = VestingParam
               { beneficiary = gpBeneficiary gp
               , deadline    = gpDeadline gp
               }
       tx = Constraints.mustPayToTheScript () $ Ada.lovelaceValueOf $ gpAmount gp
   ledgerTx <- submitTxConstraints (typedValidator p) tx
   void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
   logInfo @String $ printf "made a gift of %d lovelace to %s with deadline %s"
       (gpAmount gp)
       (show $ gpBeneficiary gp)
       (show $ gpDeadline gp)

grab :: forall w s e. AsContractError e => POSIXTime -> Contract w s e ()
grab d = do
   now   <- currentTime
   pkh   <- ownPaymentPubKeyHash
   if now < d
       then logInfo @String $ "too early"
       else do
           let p = VestingParam
                       { beneficiary = pkh
                       , deadline    = d
                       }
           utxos <- utxosAt $ scrAddress p
           if Map.null utxos
               then logInfo @String $ "no gifts available"
               else do
                   let orefs   = fst <$> Map.toList utxos
                       lookups = Constraints.unspentOutputs utxos      <>
                                 Constraints.otherScript (validator p)
                       tx :: TxConstraints Void Void
                       tx      = mconcat [Constraints.mustSpendScriptOutput oref unitRedeemer | oref <- orefs] <>
                                 Constraints.mustValidateIn (from now)
                   ledgerTx <- submitTxConstraintsWith @Void lookups tx
                   void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
                   logInfo @String $ "collected gifts"

endpoints :: Contract () VestingSchema Text ()
endpoints = awaitPromise (give' `select` grab') >> endpoints
 where
   give' = endpoint @"give" give
   grab' = endpoint @"grab" grab

mkSchemaDefinitions ''VestingSchema

mkKnownCurrencies []


We can now test this in Plutus Playground.











Looking at the wallet setup:

















Genesis Slot 0 looks like:






Slot 1, TX 0:









Slot 3, TX 0:








Slot 12, TX 0:












Slot 12, TX 1:












Final Balances:

















Cardano Testnet



We will now look at the Cardano CLI and how it interacts with the testnet. Hopefully at this point your local node is now synced from work done in the section “preparation for lecture 3”.
First, taking a quick look at some popular commands:
cardano-cli The set of cardano-cli commands include:
address: payment address commands
stake-address: stake address commands
transaction: transaction commands
node: node operation commands
stake-pool: stake pool commands
query: node query commands. Commands in this group query the local node whose Unix domain socket is obtained from the CARDANO_NODE_SOCKET_PATH environment variable.
genesis: genesis block commands
text-view: commands for dealing with text view files that are stored on disk, such as transactions or addresses
governance: governance commands
cardano-cli address The address command contains the following subcommands:
key-gen: creates a single address key pair
key-hash: prints the hash of an address to stdout
build: builds a payment address, with optional delegation to a stake address
build-script: builds a token locking script
info: prints details about the address
cardano-cli stake-address The stake-address command contains the following subcommands:
key-gen: creates a single address key pair
build: builds a stake address
key-hash: prints the hash of a stake verification key
registration-certificate: creates a registration certificate
delegation-certificate: creates a stake address delegation certificate
deregistration-certificate: creates a de-registration certificate
 
cardano-cli transaction The transaction command contains the following subcommands:
build-raw: builds a low-level transaction (uses the --cardano-mode, --byron-mode, --shelley-mode flags)
build: builds an automatically balanced transaction (automatically calculates fees)
sign: signs the transaction
assemble: combines and assembles the transaction witness(es) with a transaction body to create a transaction
witness: witnesses a transaction
submit: submits the transaction to the local node whose Unix domain socket is obtained from the CARANO_NODE_SOCKET_PATH environment variable (uses the --cardano-mode, --byron-mode, --shelley-mode flags)
calculate-min-fee: calculates the minimum fee for the transaction
calculate-min-required-utxo: calculates the minimum required ADA for a transaction output
hash-script-data: calculates the hash of script data (datums)
txid: retrieves the transaction ID
policyid: retrieves the policy ID
view: pretty prints a transaction
cardano-cli node The node command contains the following subcommands:
key-gen: creates a key pair for a node operator's offline key and a new certificate issue counter
key-gen-KES: creates a key pair for a node KES operational key
key-gen-VRF: creates a key pair for a node VRF operational key
key-hash-VRF: creates a key hash for a node VRF operational key
new-counter: keeps track of the number of KES evolutions for a given operational certificate hot key
issue-op-cert: issues a node operational certificate
cardano-cli stake-pool The stake-pool command contains the following subcommands:
registration-certificate: creates a stake pool registration certificate
de-registration-certificate: creates a stake pool de-registration certificate
id: builds pool id from the offline key
metadata-hash: retrieves the metadata hash
 
 
cardano-cli query The query command contains the following subcommands:
protocol-parameters (advanced): retrieves the node's current pool parameters (a raw dump of Ledger.ChainDepState).
tip: gets the node's current tip (slot number, hash, and block number)
stake-pools: gets the node's current set of stake pool ids
utxo: retrieves the node's current UTxO, filtered by address
ledger-state (advanced): dumps the current state of the node (a raw dump of Ledger.NewEpochState)* stake-distribution: gets the node's current set of stake pool ids
protocol-state (advanced): dumps the node's current protocol state
stake-address-info: gets the current delegations and reward accounts filtered by stake address.
stake-distribution: gets the node's current aggregated stake distribution
stake-snapshot (advanced): gets the stake snapshot information for a stake pool
pool-params (advanced): gets the current and future parameters for a stake pool
leadership-schedule: gets the slots in which the node is slot leader for the current or following epoch
kes-period-info (advanced): returns diagnostic information about your operational certificate
cardano-cli governance The governance command contains the following subcommands:
create-mir-certificate: creates an MIR (move instantaneous rewards) certificate
create-update-proposal: creates an update proposal
create-genesis-key-certificate: retrieves the genesis key certificate
 
 
 
 
 
 
 
 
cardano-cli genesis The genesis command contains the following subcommands:
key-gen-genesis: creates a genesis key pair
key-gen-delegate: creates a genesis delegate key pair
key-gen-utxo: creates a genesis UTxO key pair
key-hash: prints the identifier, or hash, of a public key
get-ver-key: derives verification key from a signing key
initial-addr: gets the address for an initial UTxO based on the verification key
initial-txin: gets the transaction ID for an initial UTxO based on the verification key.
create: creates a genesis file from a genesis template, as well as genesis keys, delegation keys, and spending keys.
create-staked: creates a staked genesis file
hash: retrieves the hash value
cardano-cli text-view The text-view command contains the following subcommand:
decode-cbor: prints a text view file as decoded CBOR.
 
In order to test our contracts, we first need to generate key pairs on the testnet. We can start by opening a new terminal to run nix-shell, making sure not to close the node syncing in the other terminal:

totinj@penguin:~/plutus-apps$ nix-shell



Head to week03 subfolder in the plutus pioneer directory, then inside that the testnet folder. We will first generate our public and private keys 01.vkey and 01.skey respectively with the command:
[nix-shell:~/plutus-pioneer-program/code/week03/testnet]$ 
cardano-cli address key-gen --verification-key-file 01.vkey --signing-key-file 01.skey




We will now generate our second public and private keys 02.vkey and 02.skey respectively with the command:
[nix-shell:~/plutus-pioneer-program/code/week03/testnet]$ 
cardano-cli address key-gen --verification-key-file 02.vkey --signing-key-file 02.skey


Looking at 01.vkey:
[nix-shell:~/plutus-pioneer-program/code/week03/testnet]$ 
cat 01.vkey

Output:
{
    "type": "PaymentVerificationKeyShelley_ed25519",
    "description": "Payment Verification Key",
    "cborHex": "58201dd3552d73e7fef875031da2b2deeacc8cc9d1d70751850408d51a4061dd3e96"
}


We can now generate an address on the testnet for 01.vkey, and output it into the file 01.addr with the following command:
[nix-shell:~/plutus-pioneer-program/code/week03/testnet]$ 
cardano-cli address build --payment-verification-key-file 01.vkey --testnet-magic 1097911063 --out-file 01.addr


Looking at 01.addr:
[nix-shell:~/plutus-pioneer-program/code/week03/testnet]$ 
cat 01.addr

Output:
addr_test1vpvlskugythmdnutq2745am2ss8sfmhz25dr7zgx8t5cjcqkw2m3l



We can now generate an address on the testnet for 02.vkey, and output it into the file 02.addr with the following command:

[nix-shell:~/plutus-pioneer-program/code/week03/testnet]$ 
cardano-cli address build --payment-verification-key-file 02.vkey --testnet-magic 1097911063 --out-file 02.addr


Looking at 02.addr:
[nix-shell:~/plutus-pioneer-program/code/week03/testnet]$ 
cat 02.addr

Output:
addr_test1vrqv87nzpwwd5q4x3ecx38ds8l3suheumc49dgvu3x9emmgvaw5kp


We now need to generate some ADA to send to our first address. This can be done from the following page using the Cardano faucet. 

https://testnets.cardano.org/en/testnets/cardano/tools/faucet/


Important to note here, that your address for 01.addr will be different then address generated in this tutorial! Make sure you send the testnet ADA to the address you generated in the CLI!

In order to query the blockchain in order to see if the funds arrived, we first need to run the command:

[nix-shell:~/plutus-pioneer-program/code/week03/testnet]$ 
export CARDANO_NODE_SOCKET_PATH=node.socket










Now we should be able to query the address. Important to note here, your local node must be in sync with the blockchain at this point or you will not be able to see the funds!

[nix-shell:~/plutus-pioneer-program/code/week03/testnet]$ 
cardano-cli query utxo --address $(cat 01.addr) --testnet-magic 1097911063

Output:
                           TxHash                                 TxIx        Amount
---------------------------------------------------------------------------
a5f29c533d8da891f05e53e8b4cd0e7beb0674245464df8b98a15d38184c8baa     0        1000000000 lovelace + TxOutDatumNone


We will now send some money to our second address using the premade script send.sh:

[nix-shell:~/plutus-pioneer-program/code/week03/testnet]$ 
cat send.sh

Output:
    --alonzo-era \
    --testnet-magic 1097911063 \
    --change-address $(cat 01.addr) \
    --tx-in a5f29c533d8da891f05e53e8b4cd0e7beb0674245464df8b98a15d38184c8baa#0 \
    --tx-out "$(cat 02.addr) 10000000 lovelace" \
    --out-file tx.body

cardano-cli transaction sign \
    --tx-body-file tx.body \
    --signing-key-file 01.skey \
    --testnet-magic 1097911063 \
    --out-file tx.signed

cardano-cli transaction submit \
    --testnet-magic 1097911063 \
    --tx-file tx.signed

Important note, you need to change the tx-in hash, to the tx hash of the 01.addr where we sent the funds in the previous step! Also note it ends with #0 specifying the transaction index of 0.

[nix-shell:~/plutus-pioneer-program/code/week03/testnet]$ 
./send.sh

Output:
Estimated transaction fee: Lovelace 165721
Transaction successfully submitted.


After waiting roughly 20 seconds, we can query the first address to see if the funds have been sent:

[nix-shell:~/plutus-pioneer-program/code/week03/testnet]$ 
cardano-cli query utxo --address $(cat 01.addr) --testnet-magic 1097911063

Output:
                           TxHash                                 TxIx        Amount
---------------------------------------------------------------------------
ea5f29c533d8da891f05e53e8b4cd0e7beb0674245464df8b98a15d38184c8baa     0        989834279 lovelace + TxOutDatumNone





[nix-shell:~/plutus-pioneer-program/code/week03/testnet]$ 
cardano-cli query utxo --address $(cat 02.addr) --testnet-magic 1097911063

Output:
                           TxHash                                 TxIx        Amount
---------------------------------------------------------------------------
eeaff45ffb4a1f06fc6e1a48fef36472d6a1323d5a90edda04d21f66dc847755     1        10000000 lovelace + TxOutDatumNone


In order to get started in Plutus using the Cardano-CLI, we need to serialize and write to disk various Plutus types. However, we first need to get the PaymentPubKeyHash of wallet 2:


[nix-shell:~/plutus-pioneer-program/code/week03/testnet]$ 
cardano-cli address key-hash --payment-verification-key-file 02.vkey --out-file 02.pkh



[nix-shell:~/plutus-pioneer-program/code/week03/testnet]$ 
cat 02.pkh

Output:
c0c3fa620b9cda02a68e70689db03fe30e5f3cde2a56a19c898b9ded


Looking at Deploy.hs, we need to replace the beneficiary payment pub key hash with the one we generated above. Note, that your hash will be different then the one in this tutorial. We also replace the deadline with a time in the future. (you can use https://www.epochconverter.com/ to find a timestamp in the future)

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Week03.Deploy
   ( writeJSON
   , writeValidator
   , writeUnit
   , writeVestingValidator
   ) where

import           Cardano.Api
import           Cardano.Api.Shelley   (PlutusScript (..))
import           Codec.Serialise       (serialise)
import           Data.Aeson            (encode)
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS
import           PlutusTx              (Data (..))
import qualified PlutusTx
import qualified Ledger

import           Week03.Parameterized

dataToScriptData :: Data -> ScriptData
dataToScriptData (Constr n xs) = ScriptDataConstructor n $ dataToScriptData <$> xs
dataToScriptData (Map xs)      = ScriptDataMap [(dataToScriptData x, dataToScriptData y) | (x, y) <- xs]
dataToScriptData (List xs)     = ScriptDataList $ dataToScriptData <$> xs
dataToScriptData (I n)         = ScriptDataNumber n
dataToScriptData (B bs)        = ScriptDataBytes bs

writeJSON :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeJSON file = LBS.writeFile file . encode . scriptDataToJson ScriptDataJsonDetailedSchema . dataToScriptData . PlutusTx.toData

writeValidator :: FilePath -> Ledger.Validator -> IO (Either (FileError ()) ())
writeValidator file = writeFileTextEnvelope @(PlutusScript PlutusScriptV1) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . Ledger.unValidatorScript

writeUnit :: IO ()
writeUnit = writeJSON "testnet/unit.json" ()

writeVestingValidator :: IO (Either (FileError ()) ())
writeVestingValidator = writeValidator "testnet/vesting.plutus" $ validator $ VestingParam
   { beneficiary = Ledger.PaymentPubKeyHash "c0c3fa620b9cda02a68e70689db03fe30e5f3cde2a56a19c898b9ded"
   , deadline    = 1645653114
   }





Open the cabal repl in the another terminal and run:

Prelude week03.Deploy> writeUnit



Prelude week03.Deploy> writeVestingValidator

Output:
Right ()



We can now generate the address of the script:

[nix-shell:~/plutus-pioneer-program/code/week03/testnet]$ 
cardano-cli address build --payment-script-file vesting.plutus --testnet-magic 1097911063 --out-file vesting.addr



[nix-shell:~/plutus-pioneer-program/code/week03/testnet]$ 
cat vesting.addr

Output:
addr_test1wzptv89prnw0tt307l09enlussrsc7n7nau4phc2kduth2gv4lsan








Looking at the give.sh script, we change the tx-in to our cat 1 address utxo that we generated earlier:

[nix-shell:~/plutus-pioneer-program/code/week03/testnet]$ 
cat give.sh

Output:
cardano-cli transaction build \
    --alonzo-era \
    --testnet-magic 1097911063 \
    --change-address $(cat 01.addr) \
    --tx-in a5f29c533d8da891f05e53e8b4cd0e7beb0674245464df8b98a15d38184c8baa#0 \
    --tx-out "$(cat vesting.addr) 200000000 lovelace" \
    --tx-out-datum-hash-file unit.json \
    --out-file tx.body

cardano-cli transaction sign \
    --tx-body-file tx.body \
    --signing-key-file 01.skey \
    --testnet-magic 1097911063 \
    --out-file tx.signed

cardano-cli transaction submit \
    --testnet-magic 1097911063 \
    --tx-file tx.signed






[nix-shell:~/plutus-pioneer-program/code/week03/testnet]$ 
./give.sh

Output:
Estimated transaction fee: Lovelace 167217
Transaction successfully submitted.


We can query the script address:

[nix-shell:~/plutus-pioneer-program/code/week03/testnet]$ 
cardano-cli query utxo --address $(cat vesting.addr) --testnet-magic 1097911063

Output:
                           TxHash                                 TxIx        Amount
---------------------------------------------------------------------------
50d9ad6558a6963d72dc25b4f37f31db15a512c708bb735a8f67f30b878bd4e3     1        200000000 lovelace + TxOutDatumHash ScriptDataInAlonzoEra "923918e403bf43c34b4ef6b48eb2ee04babed17320d8d1b9ff9ad086e86f44ec"





We also need the current slot for the next script. We can run:

[nix-shell:~/plutus-pioneer-program/code/week03/testnet]$ 
cardano-cli query tip --testnet-magic 1097911063cat vesting.addr

Output:
{
    "era": "Alonzo",
    "syncProgress": "100.00",
    "hash": "b5a4c29a91ab22789c0412eba329598f6ad5d17c7162b06112ee5da4679e5322",
    "epoch": 188,
    "slot": 51272239,
    "block": 3343799
}





Now we can look at the grab.sh script. We will alter the Txin hash to the hash of vesting.addr we queried above in the last step. We will change collateral to the hash of 02.addr from earlier. We will also alter signer-hash to the hash of 02.pkh. Finally, we need to alter invalid-before to reflect the current slot; which we queried in the last step:

[nix-shell:~/plutus-pioneer-program/code/week03/testnet]$ 
cat grab.sh

Output:
cardano-cli transaction build \
    --alonzo-era \
    --testnet-magic 1097911063 \
    --change-address $(cat 02.addr) \
    --tx-in 50d9ad6558a6963d72dc25b4f37f31db15a512c708bb735a8f67f30b878bd4e3#1 \
    --tx-in-script-file vesting.plutus \
    --tx-in-datum-file unit.json \
    --tx-in-redeemer-file unit.json \
    --tx-in-collateral eeaff45ffb4a1f06fc6e1a48fef36472d6a1323d5a90edda04d21f66dc847755#1 \
    --required-signer-hash c0c3fa620b9cda02a68e70689db03fe30e5f3cde2a56a19c898b9ded \
    --invalid-before 51272239 \
    --protocol-params-file protocol.json \
    --out-file tx.body

cardano-cli transaction sign \
    --tx-body-file tx.body \
    --signing-key-file 02.skey \
    --testnet-magic 1097911063 \
    --out-file tx.signed

cardano-cli transaction submit \
    --testnet-magic 1097911063 \
    --tx-file tx.signed







[nix-shell:~/plutus-pioneer-program/code/week03/testnet]$ 
./grab.sh

Output:
Estimated transaction fee: Lovelace 365397
Transaction successfully submitted.



After waiting roughly 20 seconds, we can query the second address to finally see if the funds have been received from the gift:


[nix-shell:~/plutus-pioneer-program/code/week03/testnet]$ 
cardano-cli query utxo --address $(cat 02.addr) --testnet-magic 1097911063

Output:
                           TxHash                                 TxIx        Amount
---------------------------------------------------------------------------
61644770e875457981d69dc3f6344a358996ea848a03e4ec17c5017071ec468b     0        199634603 lovelace + TxOutDatumNone
eeaff45ffb4a1f06fc6e1a48fef36472d6a1323d5a90edda04d21f66dc847755     1        10000000 lovelace + TxOutDatumNone







Homework Part 1


-- This should validate if either beneficiary1 has signed the transaction and the current slot is before or at the deadline
-- or if beneficiary2 has signed the transaction and the deadline has passed.


The first part of the homework, we need to write a validator function that will return true if the beneficiary1 signed the transaction and the current slot is before or at the deadline. It also must return true if the beneficiary2 signed the transaction and the deadline has passed.
We first need to pass the datum (dat) and context (ctx) into the validator:
mkValidator :: VestingDatum -> () -> ScriptContext -> Bool
mkValidator dat () ctx


Then we need to write the logic that satisfies both conditions that were described above:
   | (unPaymentPubKeyHash (beneficiary1 dat) `elem` sigs) && (to       (deadline dat) `contains` range) = True
   | (unPaymentPubKeyHash (beneficiary2 dat) `elem` sigs) && (from (1 + deadline dat) `contains` range) = True
   | otherwise                                                                                          = False
 where
   info :: TxInfo
   info = scriptContextTxInfo ctx

   sigs :: [PubKeyHash]
   sigs = txInfoSignatories info

   range :: POSIXTimeRange
   range = txInfoValidRange info




We check both conditions, one where the beneficiary1 signs and it is before or at the deadline, and also where beneficiary2 signs and it is after the deadline. Otherwise all else returns false.
The code should look like:
{-# INLINABLE mkValidator #-}
-- This should validate if either beneficiary1 has signed the transaction and the current slot is before or at the deadline
-- or if beneficiary2 has signed the transaction and the deadline has passed.
mkValidator :: VestingDatum -> () -> ScriptContext -> Bool
mkValidator dat () ctx
   | (unPaymentPubKeyHash (beneficiary1 dat) `elem` sigs) && (to       (deadline dat) `contains` range) = True
   | (unPaymentPubKeyHash (beneficiary2 dat) `elem` sigs) && (from (1 + deadline dat) `contains` range) = True
   | otherwise                                                                                          = False
 where
   info :: TxInfo
   info = scriptContextTxInfo ctx

   sigs :: [PubKeyHash]
   sigs = txInfoSignatories info

   range :: POSIXTimeRange
   range = txInfoValidRange info




Testing in Plutus Playground we see:






Looking at the wallet setup:







Slot 0, Tx 0









Slot 1, Tx 0







Slot 1, Tx 1







Slot 6, Tx 0










Slot 7, Tx 0









Final Balances:
















Homework Part 2


The second part of the homework, we need to write the validator function for the vesting contract in which we instead  pass pubkeyhash as the parameter, and POSIXTime as the datum.
mkValidator :: PaymentPubKeyHash -> POSIXTime -> () -> ScriptContext -> Bool


We can start by checking if the beneficiary’s signature exists, and also the deadline has been reached. First we pass:
mkValidator pkh s () ctx =


Now we grab the logic as described above:
   traceIfFalse "beneficiary's signature missing" checkSig      &&
   traceIfFalse "deadline not reached"            checkDeadline
 where
   info :: TxInfo
   info = scriptContextTxInfo ctx

   checkSig :: Bool
   checkSig = unPaymentPubKeyHash pkh `elem` txInfoSignatories info

   checkDeadline :: Bool
   checkDeadline = from s `contains` txInfoValidRange info


Implementing the compilation:
typedValidator :: PaymentPubKeyHash -> Scripts.TypedValidator Vesting
typedValidator p = Scripts.mkTypedValidator @Vesting
   ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode p)
   $$(PlutusTx.compile [|| wrap ||])
 where
   wrap = Scripts.wrapValidator @POSIXTime @()

Finally the boilerplate code for validator and address. Here we need to add PaymentPubKeyHash:

validator :: PaymentPubKeyHash -> Validator
validator = Scripts.validatorScript . typedValidator

scrAddress :: PaymentPubKeyHash -> Ledger.Address
scrAddress = scriptAddress . validator


The code should look like:
{-# INLINABLE mkValidator #-}
mkValidator :: PaymentPubKeyHash -> POSIXTime -> () -> ScriptContext -> Bool
mkValidator pkh s () ctx =
   traceIfFalse "beneficiary's signature missing" checkSig      &&
   traceIfFalse "deadline not reached"            checkDeadline
 where
   info :: TxInfo
   info = scriptContextTxInfo ctx

   checkSig :: Bool
   checkSig = unPaymentPubKeyHash pkh `elem` txInfoSignatories info

   checkDeadline :: Bool
   checkDeadline = from s `contains` txInfoValidRange info

data Vesting
instance Scripts.ValidatorTypes Vesting where
   type instance DatumType Vesting = POSIXTime
   type instance RedeemerType Vesting = ()

typedValidator :: PaymentPubKeyHash -> Scripts.TypedValidator Vesting
typedValidator p = Scripts.mkTypedValidator @Vesting
   ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode p)
   $$(PlutusTx.compile [|| wrap ||])
 where
   wrap = Scripts.wrapValidator @POSIXTime @()

validator :: PaymentPubKeyHash -> Validator
validator = Scripts.validatorScript . typedValidator

scrAddress :: PaymentPubKeyHash -> Ledger.Address
scrAddress = scriptAddress . validator


The Plutus Playground simulation should look like:



Slot 0, Tx 0










Slot 1, Tx 0








Slot 2, Tx 0








Slot 12, Tx 0











Slot 22, Tx 0










Final Balance:


