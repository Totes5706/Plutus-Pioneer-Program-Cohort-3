Plutus Pioneer Program - Cohort 3
January 20, 2022

Contributed By:
[Joe Totes](https://github.com/Totes5706)

Offical Video by Lars Brünjes: [PPP-Cohort3-Lecture2](https://youtu.be/BEr7lcCPjnA)

Google Doc version can be found [HERE](https://docs.google.com/document/d/1wclIpwHW-Lo8R8IJHbjvEYjyzsm5qLUMcrN9bpCOL00/edit#)


# Lecture 2: Low and High Level Validation Scripts


## Table of Contents
- [Lecture 2: Low and High Level Validation Scripts](#lecture-2-low-and-high-level-validation-scripts)
  - [Table of Contents](#table-of-contents)
  - [Preparation for Lecture 2](#preparation-for-lecture-2)
  - [Low Level Untyped Validation Scripts](#low-level-untyped-validation-scripts)
  - [High Level Typed Validation Scripts](#high-level-typed-validation-scripts)
  - [Homework Part 1](#homework-part-1)
  - [Homework Part 2](#homework-part-2)

## Preparation for Lecture 2

Before we can get started in lecture 2, we first must get our development environment up to date. You can copy and paste any of the code in this guide directly into your terminal or IDE.

First, head to the plutus-pioneer-program directory to grab the lecture week 2 contents. Execute: 

```
totinj@penguin:~/plutus-pioneer-program$ git pull
```

You can now navigate to the current week02 directory and open the cabal.project file:

```
totinj@penguin:~/plutus-pioneer-program/code/week02$ cat cabal.project
```

 Grab the plutus-apps tag inside the cabal.project file:
 
```
location: https://github.com/input-output-hk/plutus-apps.git
  tag:6aff97d596ac9d59460aab5c65627b1c8c0a1528
```

Head back to  to the plutus-apps directory and update it to the  current git tag:

```
totinj@penguin:~/plutus-apps$ git checkout main
```
```
totinj@penguin:~/plutus-apps$ git pull
```
```
totinj@penguin:~/plutus-apps$ git checkout 6aff97d596ac9d59460aab5c65627b1c8c0a1528
```

You should now be up to date and can run nix-shell in this directory. Run nix-shell:

```
totinj@penguin:~/plutus-apps$ nix-shell
```

Head back to the week02 folder to start running the cabal commands:

```
[nix-shell:~/plutus-pioneer-program/code/week02]$ cabal update
```
```
[nix-shell:~/plutus-pioneer-program/code/week02]$ cabal build
```
```
[nix-shell:~/plutus-pioneer-program/code/week02]$ cabal repl
```

If successful,  you should now be ready to start the lecture:

```haskell
Ok, 9 modules loaded.
Prelude week02.Burn > 
```

## Low Level Untyped Validation Scripts



This lecture will be focused on the on-chain code of a plutus script. There are three pieces of data that a Plutus script recieves:

	1. The datum sitting at the UTxO
	2. The redeemer coming from the input and validation
	3. The context of the transaction being validated from its I/O

These three pieces of data need to be represented by a Haskell data type. Looking at the low level implementation, the same data type will be used for all three pieces of data. In the next section, we will look at high level validation which will look at custom data types for the datum and redeemer. High level validation will come at a cost to performance.














Looking at the data for a redeemer:

data Data
A generic "data" type.
The main constructor Constr represents a datatype value in sum-of-products form: Constr i args represents a use of the ith constructor along with its arguments.
The other constructors are various primitives.
Constructors
Constr Integer [Data]
 
Map [(Data, Data)]
 
List [Data]
 
I Integer
 
B ByteString
 



We can now use the repl to get some hands-on experience. First, let's import PlutusTx

Prelude week02.Burn > import PlutusTx


Now we can get information about data using the command:

Prelude PlutusTx week02.Burn > :i Data

Output:



Example :

Prelude PlutusTx week02.Burn > I 42

Output:
I 42



Prelude PlutusTx week02.Burn > :t I 42

Output:
I 42 :: Data



We can now use this extension (-XOverloadedStrings) in order to use literal strings for other string-like types. One example is the Byte string type. Execute:

Prelude PlutusTx week02.Burn > :set -XOverloadedStrings



Example using the B constructor:

Prelude PlutusTx week02.Burn > B "Haskell"

Output:
B "Haskell"


Prelude PlutusTx week02.Burn > :t B "Haskell"

Output:
B "Haskell" :: Data

Example using Map:

Prelude PlutusTx week02.Burn > 
:t Map [(I 42, B "Haskell"), (List [I 0], I 1000)]

Output:
Map [(I 42, B "Haskell"), (List [I 0], I 1000)] :: Data



With this knowledge, we can now create our first validator. We will be using the Gift.hs file included in the week02 folder.

Looking at validation part of Gift.hs:

{-# INLINABLE mkValidator #-}
mkValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidator _ _ _ = ()

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidator ||])

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash validator

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator


This is the most basic validator function. The file is called a gift because if anyone sends funds to this script address, then anyone else can consume that output to use.








We first look at mkValidatorScript:

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidator ||])


We have a haskell function that has the logic,  where the (||) Oxford brackets convert that to a syntactical representation of that function. The compiler takes that representation and turns it into a corresponding plutus core function. Then the ($$) takes that Plutus core and splices it into the source code. That result is what then turns into the validator.  

Where mkValidatorScript is:
mkValidatorScript :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ()) -> Validator

To make use of this we need to add a Pragma to out make validator function:

{-# INLINABLE mkValidator #-}






Now we can load this file in the repl:
 
Prelude PlutusTx week02.Burn > :l src/Week02/Gift.hs

Output:
Ok, one module loaded.


Make sure both PlutusTx and Ledger.Scripts are imported”


Prelude week02.Gift > import PlutusTx


Prelude PlutusTx week02.Burn > import Ledger.Scripts


Type validator:

Prelude PlutusTx Ledger.Scripts week02.Gift > validator

Output:
Validator { <script> }












Where Validator and script are:


newtype Validator
Validator is a wrapper around Scripts which are used as validators in transaction outputs.
Constructors
Validator
 
getValidator :: Script




newtype Script
A script on the chain. This is an opaque type as far as the chain is concerned.
Constructors
Script
 
unScript :: Program DeBruijn DefaultUni DefaultFun ()










We can now run unScript $ getValidator validator:

Prelude PlutusTx Ledger.Scripts week02.Gift >
unScript $ getValidator validator

Output:
Program () (Version () 1 0 0) (Apply () (Apply () (LamAbs () (DeBruijn {dbnIndex = 0}) (LamAbs () (DeBruijn {dbnIndex = 0}) (LamAbs () (DeBruijn {dbnIndex = 0}) (LamAbs () (DeBruijn {dbnIndex = 0}) (LamAbs () (DeBruijn {dbnIndex = 0}) (Var () (DeBruijn {dbnIndex = 5}))))))) (Delay () (LamAbs () (DeBruijn {dbnIndex = 0}) (Var () (DeBruijn {dbnIndex = 1}))))) (LamAbs () (DeBruijn {dbnIndex = 0}) (Var () (DeBruijn {dbnIndex = 1}))))



This is the plutus core script in this representation. We compiled our mkValidator function and turned it into Plutus Core.

The other two important parts of the validator are the validatorHash and ScriptAddress functions.

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash validator

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator


Where valHash stores the hash of the validator and scrAddress stores the address of the script.

Example:

Prelude PlutusTx Ledger.Scripts week02.Gift > valHash

Output:
67f33146617a5e61936081db3b2117cbf59bd2123748f58ac9678656


Prelude PlutusTx Ledger.Scripts week02.Gift > scrAddress

Output:
Address {addressCredential = ScriptCredential 67f33146617a5e61936081db3b2117cbf59bd2123748f58ac9678656, addressStakingCredential = Nothing}


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












Genesis Slot 0 looks like:









Slot 1, TX 0:




 

Slot 1, TX 1:








Slot 2, TX 0:








Final Balances:


















We now look at the file Burn.hs where mkValidator looks like:

mkValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidator _ _ _ = traceError "BURNT!"


Load the file and check for errors:
Prelude PlutusTx week02.Gift > :l src/Week02/Burn.hs

Output:
Ok, one module loaded.


You should now be able to successfully compile and run the burn contract by copy/pasting it into Plutus Playground and using the two buttons in the top right corner: “Compile” and “Simulate”: 



























Evaluating the wallets with the same configuration as gift.hs:











Genesis Slot 0 looks like:





Slot 1, TX 0:









Slot 1, TX 1:









Final Balances:




As expected, the grab did not work. No transactions can ever use those outputs as inputs. 

Contract instance stopped with error: "WalletError (ValidationError (ScriptFailure (EvaluationError [\"BURNT!\"] \"CekEvaluationFailure\")))" ]












High Level Typed Validation Scripts



We will now take a look at some examples of high level typed validation scripts. We can start by looking at Typed.hs:


Prelude PlutusTx week02.Burn > :l src/Week02/Typed.hs

Output:
Ok, one module loaded.



We The mkValidator function inside Typed.hs looks like:

mkValidator :: () -> Integer -> ScriptContext -> Bool
mkValidator _ r _ = traceIfFalse "wrong redeemer" $ r == 42




This redeemer will check to see if the integer amount is 42, otherwise it will return false, outputting “wrong redeemer”.

We then modified the compilation function:

data Typed
instance Scripts.ValidatorTypes Typed where
   type instance DatumType Typed = ()
   type instance RedeemerType Typed = Integer

typedValidator :: Scripts.TypedValidator Typed
typedValidator = Scripts.mkTypedValidator @Typed
   $$(PlutusTx.compile [|| mkValidator ||])
   $$(PlutusTx.compile [|| wrap ||])
 where
   wrap = Scripts.wrapValidator @() @Integer



We first declare DatumType as type unit () and RedeemerType as an Integer. We then add a wrap function to be able to translate the strong types from the low level version. It is then declared in the where, that the datum and redeemer is  of type () and Integer respectively.

We can now look at a practical example in Plutus Playground. First let’s check to make sure there are no errors in the file isData.hs.

Prelude PlutusTx week02.Typed > :l src/Week02/isData.hs

Output:
Ok, one module loaded.



Looking at the on-chain validating code:
{-# INLINABLE mkValidator #-}
mkValidator :: () -> MySillyRedeemer -> ScriptContext -> Bool
mkValidator _ (MySillyRedeemer r) _ = traceIfFalse "wrong redeemer" $ r == 42

data Typed
instance Scripts.ValidatorTypes Typed where
   type instance DatumType Typed = ()
   type instance RedeemerType Typed = MySillyRedeemer

typedValidator :: Scripts.TypedValidator Typed
typedValidator = Scripts.mkTypedValidator @Typed
   $$(PlutusTx.compile [|| mkValidator ||])
   $$(PlutusTx.compile [|| wrap ||])
 where
   wrap = Scripts.wrapValidator @() @MySillyRedeemer

validator :: Validator
validator = Scripts.validatorScript typedValidator

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash typedValidator

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator



You should now be able to successfully compile and run the isData contract by copy/pasting it into Plutus Playground and using the two buttons in the top right corner: “Compile” and “Simulate”: 

Our first test case will use a grab value of 100. This should be expected to fail and the money should not be transfered.









Results:


 As expected, the grab did not happen.
Our second test case will use a value that is 42. This should be expected to pass validation.










Results:



As expected, the grab was a success and the money was transfered.

Homework Part 1

-- This should validate if and only if the two Booleans in the redeemer are equal!

mkValidator :: () -> (Bool, Bool) -> ScriptContext -> Bool
mkValidator _ _ _ = True -- FIX ME!


The goal of homework part 1 is to have the mkValidator pass only if the two booleans in the redeemer are equal.
First, we need to pass the correct parameters into mkValidator. It accepts a unit type (), followed by two booleans we can call b and c respectively.

mkValidator :: () -> (Bool, Bool) -> ScriptContext -> Bool
mkValidator () (b, c) _ = traceIfFalse "wrong redeemer" $ b == c


Next, we check whether the b and c are equal in value; otherwise throw the message “wrong redeemer”.
Then, we need to declare the data types for both unit and boolean parameters.
data Typed
instance Scripts.ValidatorTypes Typed where
   type instance DatumType Typed = ()
   type instance RedeemerType Typed = (Bool, Bool)


Next, we write the compilation code for a high level validation script, wrapping both the unit type and the boolean values.
typedValidator :: Scripts.TypedValidator Typed
typedValidator = Scripts.mkTypedValidator @Typed
   $$(PlutusTx.compile [|| mkValidator ||])
   $$(PlutusTx.compile [|| wrap ||])
 where
   wrap = Scripts.wrapValidator @() @(Bool, Bool)



Lastly, we write the boiler plate code for the validator, valHash, and srcAddress.
validator :: Validator
validator = Scripts.validatorScript typedValidator

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash typedValidator

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator



The final on-chain code should look like:
{-# INLINABLE mkValidator #-}
-- This should validate if and only if the two Booleans in the redeemer are equal!
mkValidator :: () -> (Bool, Bool) -> ScriptContext -> Bool
mkValidator () (b, c) _ = traceIfFalse "wrong redeemer" $ b == c

data Typed
instance Scripts.ValidatorTypes Typed where
   type instance DatumType Typed = ()
   type instance RedeemerType Typed = (Bool, Bool)

typedValidator :: Scripts.TypedValidator Typed
typedValidator = Scripts.mkTypedValidator @Typed
   $$(PlutusTx.compile [|| mkValidator ||])
   $$(PlutusTx.compile [|| wrap ||])
 where
   wrap = Scripts.wrapValidator @() @(Bool, Bool)

validator :: Validator
validator = Scripts.validatorScript typedValidator

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash typedValidator

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator


Testing the code in Plutus Playground:












Results:


As expected, validation passed when both booleans were equal in value.

Homework Part 2


The goal of homework part 2 is the same objective as part, with the exception of using custom data types for the redeemer:

data MyRedeemer = MyRedeemer
   { flag1 :: Bool
   , flag2 :: Bool
   } deriving (Generic, FromJSON, ToJSON, ToSchema)


The logic is the same, except now we will be using MyRedeemer to pass both flags as booleans.


mkValidator :: () -> MyRedeemer -> ScriptContext -> Bool
mkValidator () (MyRedeemer b c) _ = traceIfFalse "wrong redeemer" $ b == c


We alter the code and change the data typed from boolean to now MyRedeemer:

data Typed
instance Scripts.ValidatorTypes Typed where
   type instance DatumType Typed = ()
   type instance RedeemerType Typed = MyRedeemer


Same change inside the compilation wrapper:

typedValidator :: Scripts.TypedValidator Typed
typedValidator = Scripts.mkTypedValidator @Typed
   $$(PlutusTx.compile [|| mkValidator ||])
   $$(PlutusTx.compile [|| wrap ||])
 where
   wrap = Scripts.wrapValidator @() @MyRedeemer



The final on-chain code should look like:

data MyRedeemer = MyRedeemer
   { flag1 :: Bool
   , flag2 :: Bool
   } deriving (Generic, FromJSON, ToJSON, ToSchema)

PlutusTx.unstableMakeIsData ''MyRedeemer

{-# INLINABLE mkValidator #-}
-- This should validate if and only if the two Booleans in the redeemer are equal!
mkValidator :: () -> MyRedeemer -> ScriptContext -> Bool
mkValidator () (MyRedeemer b c) _ = traceIfFalse "wrong redeemer" $ b == c

data Typed
instance Scripts.ValidatorTypes Typed where
   type instance DatumType Typed = ()
   type instance RedeemerType Typed = MyRedeemer

typedValidator :: Scripts.TypedValidator Typed
typedValidator = Scripts.mkTypedValidator @Typed
   $$(PlutusTx.compile [|| mkValidator ||])
   $$(PlutusTx.compile [|| wrap ||])
 where
   wrap = Scripts.wrapValidator @() @MyRedeemer

validator :: Validator
validator = Scripts.validatorScript typedValidator

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash typedValidator

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator








Testing the code in Plutus Playground:
















Results:



As expected, validation passed when both booleans were equal in value