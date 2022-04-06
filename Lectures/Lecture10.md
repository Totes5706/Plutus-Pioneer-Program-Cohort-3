# Lecture 10: Staking and the Private Testnet

Plutus Pioneer Program - Cohort 3 
March 30th, 2022

Contributed By:
[Joe Totes](https://github.com/Totes5706)


Offical Video by Lars Brünjes: [PPP-Cohort3-Lecture9](https://youtube.com/playlist?list=PLNEK_Ejlx3x3EV7FKhlogJgS27dWgwI9B)


## Table of Contents

- [Lecture 10: Staking and the Private Testnet](#lecture-10-staking-and-the-private-testnet)
  - [Table of Contents](#table-of-contents)
  - [Preparation for Lecture 10](#preparation-for-lecture-10)
  - [Introduction](#introduction)
  - [The Private Testnet](#the-private-testnet)
  - [Plutus and Staking](#plutus-and-staking)
  - [Trying it on the Testnet](#trying-it-on-the-testnet)
  - [Conclusion](#conclusion)
  
## Preparation for Lecture 10

Before we can get started in lecture 10, we first must get our development environment up to date. You can copy and paste any of the code in this guide directly into your terminal or IDE.

First, head to the plutus-pioneer-program directory to grab the lecture week 10 contents. </br>
**This week is slightly different because we need to git pull from multiple repos. Go into the week 10 folder and then execute:**

```
totinj@penguin:~/plutus-pioneer-program$ git pull 
```

Head into he week10 subfolder and clone the woofpool private testnet:

```
totinj@penguin:~/plutus-pioneer-program/code/week10$ rmdir cardano-private-testnet-setup
```

```
totinj@penguin:~/plutus-pioneer-program/code/week10$ git clone https://github.com/woofpool/cardano-private-testnet-setup.git
```

Open the cabal.project file:

```
totinj@penguin:~/plutus-pioneer-program/code/week10$ cat cabal.project
```

Grab the plutus-apps tag inside the cabal.project file:

```
location: https://github.com/input-output-hk/plutus-apps.git
  tag:14bed17e8608162ee81969e482c1815fb78bd7b0
```
Head back to the plutus-apps directory. Now we can update plutus-apps to the current git tag:

```
totinj@penguin:~/plutus-apps$ git checkout main
```
```
totinj@penguin:~/plutus-apps$ git pull
```
```
totinj@penguin:~/plutus-apps$ git checkout 14bed17e8608162ee81969e482c1815fb78bd7b0
```
**We will need to account for a bug here before we run nix-shell. In the main plutus-apps directory, open shell.nix and delete the line ```cardano-wallet.cardano-wallet``` and click save:**

```haskell
 # local build inputs ( -> ./nix/pkgs/default.nix )
  localInputs = (with plutus-apps; [
    cabal-install
    cardano-node.cardano-cli
    cardano-node.cardano-node
    cardano-wallet.cardano-wallet   -- DELETE THIS LINE XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX   
    cardano-repo-tool
    docs.build-and-serve-docs
    fixPngOptimization
    fix-purs-tidy
    fixStylishHaskell
    haskell-language-server
    haskell-language-server-wrapper
    hie-bios
    hlint
    pab-nami-demo.generate-purescript
    pab-nami-demo.start-backend
    plutus-playground.generate-purescript
    plutus-playground.start-backend
    psa
    purescript-language-server
    purs
    purs-tidy
    spago
    spago2nix
    stylish-haskell
    updateMaterialized
    updateClientDeps
  ]);
```

You should now be up to date and can run nix-shell in this directory. Run nix-shell:

```
totinj@penguin:~/plutus-apps$ nix-shell
```

Head back to the week10 folder to start running the cabal commands:

```
[nix-shell:~/plutus-pioneer-program/code/week10]$ cabal update
```
```
[nix-shell:~/plutus-pioneer-program/code/week10]$ cabal build
```

You should now be able to start the lecture.

## Introduction

In previous iterations of this course, in the tenth lecture, we did another walkthrough where we showed how to create a smart contract, test it, write on-chain and off-chain code and then finally deploy it with the PAB. However, that was before the Alonzo hard fork, so we could not try things on the real blockchain. We instead just used the PAB simulator.

Now after Alonzo, it would be much nicer to show something on the real blockchain. Unfortunately, the PAB is still not in a state where this is easily doable. We have already done something similar in lecture six where we showed how to use the PAB to mint a token on the testnet.

Some of you asked about staking in Plutus, so we decided to do that instead which was not really possible before the Alonzo hard fork. All the mechanisms were there already, however there was no way to try it out because all we had were the plutus playground and the emulator. Neither of these  have a concept of staking. This is now of course different because after Alonzo, we have the testnet or the mainnet available. 

The second problem is that on the testnet and the mainnet things take quite a lot of time. If you, for example, delegate to a stake pool then it takes many days before you receive your first rewards. Also because an epoch on the mainnet and on the testnet lasts five days, it would take a couple weeks to see results. For obvious reasons we didn't think that that was suitable for this lecture.

Luckily, there is also the option to run a private testnet. The advantage of doing that is that you can use different parameters from the mainnet. So in particular, you can make epochs much shorter, so that it is much easier to try something staking related because you do not have to wait for five days before anything happens. So in this lecture we will do that; we will take a very simple example, how to use Plutus in relation to staking and demonstrate it using a private testnet with much shorter epochs.

## The Private Testnet


As always you can find the code for this lecture in the GitHub repo in the folder code/week10.

So the repository we included into our course repository via git submodule is woof pool's cardano private testnet setup.There is nice collection of scripts that make it extremely easy to quickly run your own private testnet. So there are lots of options and very nice explanations but we are just using the default setup. This will run three nodes, one stake pool, and creates one user.

In this week's code folder, there is a scripts subfolder with lots of scripts; one of these scripts is called **start-private-testnet.sh**.


```bash
#!/bin/bash

if [ -d tmp ];
then
    rm -rf tmp
fi
mkdir tmp

cd cardano-private-testnet-setup
scripts/automate.sh
```

What that does is first, it creates a temp directory for all the artifacts that we will create during our experiments. It first checks whether that folder already exists, if it does it then deletes it then it freshly created it. If it’s an empty folder, it changes directory into cardano-private-testnet-setup and then calls the automate script in that repository.

So let's do that now. So from this week's code folderwejust call that script in script start private testnet.

```
[nix-shell:~/plutus-pioneer-program/code/week10]$ ./scripts/start-private-testnet.sh


Output:
...
Nodes are running in era: "Alonzo", major protocol version: 5

Congrats! Your network is ready for use!
```

Now the testnet is created, and the stake pool is set up. Now everything is running. We should keep this running in a separate tab of our terminal so we can easily interact with it.

So in particular we have a node socket here and we can use that to interact with this testnet node with one of the tested nodes.
```
...
wait until socket is detected, socket: private-testnet/node-bft1/node.sock
...
```

As we said, we kept the default parameters of this private testnet. In particular there is one parameter kes, also known as key skeys. They have to be renewed every now and again a certain period for security reasons. Here it is configured for 91 epochs since the private testnet is running so fast after an hour or so, you actually reach that 91st epoch and then things stop if you don't renew your key skey. So if you take too much time then that could happen.

Of course you can always restart the testnet. In order to restart it, you can just interrupt it and then you should kill all cardano nodes that are still running. Afterwards, you can just restart the testnet. 

Now we can interact with it just as we would with the mainnet or the testnet. The only difference is that we must set the Cardano node socket path correctly. We must also use the correct testnet magic, which in this case is 42. All the normal Cardano-CLI commands work, let's just correctly set the node socket and the testnet magic. So looking at the script **query-tip.sh**  also in the scripts folder.

```
#!/bin/bash
export CARDANO_NODE_SOCKET_PATH=cardano-private-testnet-setup/private-testnet/node-bft1/node.sock
cardano-cli query tip --testnet-magic 42
```

This queries the tip of the blockchain. If we do that now, then we see that in my machine right now I'm already in epoch 18, in slot 9071 and in block 879.

```
[nix-shell:~/plutus-pioneer-program/code/week10]$ ./scripts/query-tip.sh


Output:
{
    "era": "Alonzo",
    "syncProgress": "100.00",
    "hash": "ea1185ebe591c5b0e885f41e30fe05d2afff760694d90fea79aacd712a2b7f47",
    "epoch": 18,
    "slot": 9071,
    "block": 879
}
```

As we mentioned before, a lot of things are already automatically created for us, in particular in this folder cardano private testnet setup private testnet addresses are various artifacts.

```
[nix-shell:~/plutus-pioneer-program/code/week10]$ ls cardano-private-testnet-setup/private-testnet/addresses/


Output:
pool-owner1-stake.addr      pool-owner1-stake.vkey  pool-owner1.vkey        user1-stake.reg.cert  user1.addr
pool-owner1-stake.reg.cert  pool-owner1.addr        user1-stake.addr        user1-stake.skey      user1.skey
pool-owner1-stake.skey      pool-owner1.skey        user1-stake.deleg.cert  user1-stake.vkey      user1.vkey
```

But in particular we see a user1 has been created with verification key, signing key, payment address, staking key pair, verification key, signing key, corresponding staking address. If you recall from last time, we talked about the Cardano-CLI, there is a very useful query command, query UTxO to get the UTxO at an address. Looking at **query-utxo-user1.sh**:

```
#!/bin/bash
export CARDANO_NODE_SOCKET_PATH=cardano-private-testnet-setup/private-testnet/node-bft1/node.sock
cardano-cli query utxo \
    --testnet-magic 42 \
    --address $(cat cardano-private-testnet-setup/private-testnet/addresses/user1.addr)
```

So using again the correct node socket path and the correct testnet magic and taking the address that we just showed you, so user1 address, we have this script.

```
[nix-shell:~/plutus-pioneer-program/code/week10]$ ./scripts/query-utxo-user1.sh

Output:
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
2207d6294a2a7b8ba664c85f18bed9d49d5837240de52547df0b101762a2a4a7     0        450000000000 lovelace + TxOutDatumNone
2207d6294a2a7b8ba664c85f18bed9d49d5837240de52547df0b101762a2a4a7     1        449999000000 lovelace + TxOutDatumNone
```


And if we execute it, we see that conveniently this user already has lots of funds. So we have 450,000 ADA and the first UTxO and almost 450,000 ADA in the second UTxO, in total almost 900,000 ADA.


Another query command the Cardano-CLI provides is query stake pools, which is the name suggests lists all stake pools. Looking at **query-stake-pools.sh**:

```
#!/bin/bash
export CARDANO_NODE_SOCKET_PATH=cardano-private-testnet-setup/private-testnet/node-bft1/node.sock
cardano-cli query stake-pools --testnet-magic 42
```

So if we execute that, we see we have one stake pool already and this is its stake pool id.

```
[nix-shell:~/plutus-pioneer-program/code/week10]$ ./scripts/query-stake-pools.sh


Output:
pool1evpz7a0jpn0pj3v3d8uwg6w2x94lj658mcpf6ltqpnhhjmxl9hv
```

There's another query command, stake address info. Looking at **query-stake-address-info-user1.sh**:

```
#!/bin/bash
export CARDANO_NODE_SOCKET_PATH=cardano-private-testnet-setup/private-testnet/node-bft1/node.sock
cardano-cli query stake-address-info \
    --testnet-magic 42 \
    --address $(cat cardano-private-testnet-setup/private-testnet/addresses/user1-stake.addr)
```

Which takes a stake address and then gives us information about that stake address. So as we saw, the user1 stake address has been created for us. 

```
[nix-shell:~/plutus-pioneer-program/code/week10]$ ./scripts/query-stake-address-info-user1.sh


Output:
[
    {
        "delegation": "pool1evpz7a0jpn0pj3v3d8uwg6w2x94lj658mcpf6ltqpnhhjmxl9hv",
        "address": "stake_test1uplxmdgzyaa6tlsvg8ga44drrt779m9rneqfvzvzqn2f8hq2jhyv4",
        "rewardAccountBalance": 1849873538
    }
]
```

Executing that script, gives us the following information that this stake address delegates to a pool, which of course, is the only pool there is. This is the stake address in question and we see that we already have quite a lot of accumulated rewards. If we count digits correctly it's at the moment 1,849 ADA. So how can we withdraw those rewards? Looking at **withdraw-user1.sh**:

```
#!/bin/bash

txin=$1
amt=$(scripts/query-stake-address-info-user1.sh | jq .[0].rewardAccountBalance)
raw=tmp/tx.raw
signed=tmp/tx.signed

echo "txin = $1"
echo "amt = $amt"

export CARDANO_NODE_SOCKET_PATH=cardano-private-testnet-setup/private-testnet/node-bft1/node.sock

cardano-cli transaction build \
    --testnet-magic 42 \
    --change-address $(cat cardano-private-testnet-setup/private-testnet/addresses/user1.addr) \
    --out-file $raw \
    --tx-in $txin \
    --withdrawal "$(cat cardano-private-testnet-setup/private-testnet/addresses/user1-stake.addr)+$amt" \

cardano-cli transaction sign \
    --testnet-magic 42 \
    --tx-body-file $raw \
    --out-file $signed \
    --signing-key-file cardano-private-testnet-setup/private-testnet/addresses/user1.skey \
    --signing-key-file cardano-private-testnet-setup/private-testnet/addresses/user1-stake.skey

cardano-cli transaction submit \
    --testnet-magic 42 \
    --tx-file $signed
```

So we wrote a script for that, which will create a appropriate transaction and that script takes one argument, a UTxO as input. As we saw there are two,  we can pick any of the two.

- Nextwelook up the amount that we can withdraw. There's a peculiarity with withdrawals in Cardano, so you can only ever withdraw the whole accumulated rewards. You cannot do partial withdrawals. This means that we must know exactly how many rewards have been accumulated when we execute this command. We use the script we used before, the **query-stake-addresses-info-user1.sh** and then we use the jq tool (standard linux tool to analyze json values and extract this reward account balance field). So amount now holds the available rewards.
- Then raw and signed are just file names for the unsigned transaction and the signed transaction.
- Then just log for debugging purposes where txin an the amount.
- Set the node socket path and now we build the transaction, sign the transaction, submit it.

### Transaction Build

- In order to build it we meed testnet magic then we must provide the change address, we again use user's address; the same one where we also take the input from. 
- We must specify the outfile for the unsigned transaction.  
- We must specify at least one UTxO as input, we use this argument we passed in.
- Finally this is a field we have not seen before in the previous transactions we built, we use this withdrawal option. That takes an address and an amount. So as address we use the  user1 stake address. For the amount the one we computed earlier, the available rewards.

### Transaction Sign

- Then we sign the transaction as always, by providing the magic, filename of the unsigned transaction, and file for the signed transaction.
- We need our payment signing key, because we are spending this UTxO here. We can only do that by proving that it's ours, so we must sign with our payment signing key. However, we are also withdrawing, so we grabbed rewards sitting at the stake address. In order to prove that we have the right to do that, we need the signing key for that stake address as well. Therefore we need two signatures here; two signing key files.

### Transaction Submit

- Finally we submit the transaction, by providing the magic and the file name of the file to submit.


```
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
2207d6294a2a7b8ba664c85f18bed9d49d5837240de52547df0b101762a2a4a7     0        450000000000 lovelace + TxOutDatumNone
2207d6294a2a7b8ba664c85f18bed9d49d5837240de52547df0b101762a2a4a7     1        449999000000 lovelace + TxOutDatumNone
```


In order to try that we need a UTxO as argument, so we can pick one of these two; let's pick the second one. 

```
[nix-shell:~/plutus-pioneer-program/code/week10]$ ./scripts/withdraw-user1.sh 2207d6294a2a7b8ba664c85f18bed9d49d5837240de52547df0b101762a2a4a7#1


Output:
txin = 2207d6294a2a7b8ba664c85f18bed9d49d5837240de52547df0b101762a2a4a7#1
amt = 2242740150
Estimated transaction fee: Lovelace 171441
Transaction successfully submitted.
```

We execute it, and it seems to have gone well. Here we see this debugging information, so while explaining this tutorial, we received even more rewards so now it is at 2242 ADA. That seems to have worked.

```
[nix-shell:~/plutus-pioneer-program/code/week10]$ ./scripts/query-utxo-user1.sh

Output:
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
2207d6294a2a7b8ba664c85f18bed9d49d5837240de52547df0b101762a2a4a7     0        450000000000 lovelace + TxOutDatumNone
7883b4137d93db90b4bed1d806b0d5de2deb460c7d05a188a1d2d9be6450a307     0        452241568709 lovelace + TxOutDatumNone
```

We can now check UTxOs again, and here we see this has changed. As expected, the second one was spent and the new one was created here. We see that the original 449,999 ADA are now 452,241 ADA, so the rewards have indeed been added.

```
[nix-shell:~/plutus-pioneer-program/code/week10]$ ./scripts/query-stake-address-info-user1.sh


Output:
[
    {
        "delegation": "pool1evpz7a0jpn0pj3v3d8uwg6w2x94lj658mcpf6ltqpnhhjmxl9hv",
        "address": "stake_test1uplxmdgzyaa6tlsvg8ga44drrt779m9rneqfvzvzqn2f8hq2jhyv4",
        "rewardAccountBalance": 251322789
    }
]
```


If we check the stake info, then we see that because we withdrew the amounts, they are gone. However, because we spent some time explaining, we already accumulated another 251 ADA in the rewards in the meantime.

So we have seen how we can interact with the private testnet and in particular, try out staking related functions. It is very nice and convenient, because epochs pass so fast, so we do not have to wait long for rewards to accumulate. 

But of course, none of what was explained so far has anything to do with Plutus. Next we want to look at how to combine Plutus with staking. In particular, instead of using a stake address that's based on a public private keypair, we will instead create a stake address that's based on a Plutus script. Therefore, instead of providing the signing key for the stake address, we will  provide the script file, the Plutus script. Then we can put arbitrary logic as always into the Plutus script which will then governed under which conditions we can, for example, do a withdrawal of rewards. So we will look at that next.

## Plutus and Staking

So how does the interaction between Plutus and staking work? We have talked about script purposes.

```
data ScriptPurpose

Purpose of the script that is currently running

Constructors
Minting CurrencySymbol	 
Spending TxOutRef	 
Rewarding StakingCredential	 
Certifying DCert
```

If you recall, the script context that our Plutus scripts always receive as one of the arguments contains a field of typescript purpose.And during this lecture so far, we have only looked at the first two purposes.The arguably most important one is Spending TxOutRef.

```
data TxOutRef

A reference to a transaction output. This is a pair of a transaction reference, and an index indicating which of the outputs of that transaction we are referring to.

Constructors
TxOutRef	 

    txOutRefId :: TxId 
    txOutRefIdx :: Integer

    Index into the referenced transaction's outputs
```
So when we are spending a UTxO sitting at the script address, this UTxO isidentified by its tx out reference.Then the corresponding Plutus script is executed and it receives the datum of the UTxO we want to spend, the redeemer and the script context. And then we can put arbitrary logic in the script to determine whether spending this UTxO is valid.Then we had the minting script purpose, which is for minting and burning native tokens.

So whenever a transaction mints or burns a token, then the minting script corresponding to the currency symbol of the token is executed.And again we can use arbitrary logic, the script receives a redeemer and the script context.And we can determine whether minting or burning for this transaction is valid or not.

```
Rewarding StakingCredential	 
Certifying DCert
```

But we haven't talked about  rewarding and certifying it and those two are related  to Plutus and staking.So aswebriefly mentioned before, instead of using a public private key pair to create a stake address, we can instead use a Plutus script.And then the hash of that Plutus script will give a stake address, a script stake address.And where as we saw in the previous example, ifwewant to withdraw my rewards for example,for a normal stake address,we have to as witness that I'm allowed to do that provide the signing key for this stake address.So if we use a script stake address, then instead the corresponding script will be executed.It will receive the redeemer and the script context and againwecan use arbitrary logic to determine whether this transaction is allowed to actually withdraw those rewards.

Similarly Certifying DCert.So there are various certifications thatwecan attach to a transaction.In particular, important for staking out registration delegation and deregistration certificates.So ifwenewly create a stake address,wefirst have to register it by creating a transaction that contains a registration certificate for this stake address.Then ifwewant to delegate to a pool or change an existing validationwehave to use a transaction that contains a delegation certificate.And that certificate then contains the poolwewant to delegate to.And finally we can also unregister a stake address again and get the original deposit back that Ihad to pay when we registered the stake address.So for those then if we do  a delegation for example, then again the corresponding  script will be executed and can contain arbitrary logic to determine whether this delegation for  example is legal or not.


So we want to concentrate on the rewarding purpose in this lecture.

```
data TxInfo

A pending transaction. This is the view as seen by validator scripts, so some details are stripped out.

Constructors
TxInfo	 

    txInfoInputs :: [TxInInfo]                            Transaction inputs
    txInfoOutputs :: [TxOut]                              Transaction outputs
    txInfoFee :: Value                                    The fee paid by this transaction.
    txInfoMint :: Value                                   The Value minted by this transaction.
    txInfoDCert :: [DCert]                                Digests of certificates included in this transaction
    txInfoWdrl :: Map StakingCredential Integer           Withdrawals
    txInfoValidRange :: POSIXTimeRange                    The valid range for the transaction.
    txInfoSignatories :: [PubKeyHash]                     Signatures provided with the transaction, attested that they all signed the tx 
    txInfoRedeemers :: Map ScriptPurpose Redeemer 
    txInfoData :: Map DatumHash Datum 
    txInfoId :: TxId                                      Hash of the pending transaction (excluding witnesses)
```

So if we look at the tx info field we have seen examples of various of the fields content in it like the inputs, the outputs, talked about the minted value, we talked about the valid range that allows us to argue about time, the signatures.

```
 txInfoDCert :: [DCert]                                Digests of certificates included in this transaction
 txInfoWdrl :: Map StakingCredential Integer           Withdrawals
```

But we haven't looked at these two fields here. So here we have a field with all the certificates that are attached to the transaction. And now relevant for this lecture, we have a list of pairs containing of staking credentials and integers for withdrawals.So each pair corresponds to the withdrawal of rewards from the state address given by the staking credential.So the staking credential corresponds to our staking address given by a Plutus script.And the integer is the amount of lovelace we are withdrawing. So whenever we withdraw rewards from a script staking address.Then the corresponding Plutus script will be executed and we receive this credentials argument in the script purpose.


We created module week 10.Staking to provide an example:

```haskell
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Week10.Staking
    ( stakeValidator
    ) where

import           Ledger
import           Ledger.Typed.Scripts        as Scripts
import           Plutus.V1.Ledger.Ada        (Ada (..), fromValue)
import           Plutus.V1.Ledger.Credential (StakingCredential)
import qualified PlutusTx
import           PlutusTx.Prelude

{-# INLINABLE mkStakingValidator #-}
mkStakingValidator :: Address -> () -> ScriptContext -> Bool
mkStakingValidator addr () ctx = case scriptContextPurpose ctx of
    Certifying _   -> True
    Rewarding cred -> traceIfFalse "insufficient reward sharing" $ 2 * paidToAddress >= amount cred
    _              -> False
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    amount :: StakingCredential -> Integer
    amount cred = go $ txInfoWdrl info
      where
        go :: [(StakingCredential, Integer)] -> Integer
        go [] = traceError "withdrawal not found"
        go ((cred', amt) : xs)
            | cred' == cred = amt
            | otherwise     = go xs

    paidToAddress :: Integer
    paidToAddress = foldl f 0 $ txInfoOutputs info
      where
        f :: Integer -> TxOut -> Integer
        f n o
            | txOutAddress o == addr = n + getLovelace (fromValue $ txOutValue o)
            | otherwise              = n

stakeValidator :: Address -> StakeValidator
stakeValidator addr = mkStakeValidatorScript $
    $$(PlutusTx.compile [|| wrapStakeValidator . mkStakingValidator ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode addr
```
And it's probably not a very useful realistic example.So the idea is that withdrawals are only allowed if at least half of the withdrawn rewards goes to a previously specified address.So anybody can withdraw rewards from the staking address.But half of the rewards always have to go to a previously specified address.

```haskell
{-# INLINABLE mkStakingValidator #-}
mkStakingValidator :: Address -> () -> ScriptContext -> Bool
mkStakingValidator addr () ctx = case scriptContextPurpose ctx of
    Certifying _   -> True
    Rewarding cred -> traceIfFalse "insufficient reward sharing" $ 2 * paidToAddress >= amount cred
    _              -> False
```

So let's look how we can do this.So it's a parameterized contract, so the first argument is the parameter, theaddress, that will always receive half.Next comes the redeemer so  we just use unit and then as always for Plutus scripts, the script context.And the result is a boolean indicating whether this is okay or not.So we call the address addr, unit is always unit, context ctx.And we do a case distinction on the script context purpose, the purpose of the context.So if it's certifying, we just say true.So that means we allow arbitrary delegation and the registration.For minting and spending it's false, but the interesting case is rewarding, so rewarding credential.And here we must check that what we said is satisfied so this specified address addr receives at least half of the rewards.So if not then we log this error, a trace's error, insufficient reward sharing.And we check that this here will be the amount, the amount I'm withdrawing, the total rewards.And twice of what we pay to the specified address must be greater or equal to the total rewards which the other way around means that paid to address is at least half of the total amount.So this is the logic and now we just have to compute the various things.

```haskell
   info :: TxInfo
   info = scriptContextTxInfo ctx
```
So as often before we define info which is the tx info field sitting in the script context.

```haskell
    amount :: StakingCredential -> Integer
    amount cred = go $ txInfoWdrl info
      where
        go :: [(StakingCredential, Integer)] -> Integer
        go [] = traceError "withdrawal not found"
        go ((cred', amt) : xs)
            | cred' == cred = amt
            | otherwise     = go xs
```

Now to get the amount, the total reward amount, given the staking credentials and those we receive here in the purpose and then we can use them as an argument here.So giving the staking credentials, we have this helper function that receive a list of pairs of staking credentials and integers.And we call that with the field we just looked at in the tx info this txinfo wdrl field, which contains such pairs.So if it's the empty list, then we didn't find the correct withdrawal and we trace an error.And if it's not empty so there is at least one pair with some credential and some amount, Check whether the credential is the one I'm interested in, the one I'm validating right now.And if so, then we have found my amount so we return that.And otherwise we recursively look at the tail of the list.So this amount function will give me the number of withdrawn lovelace.

```haskell
    paidToAddress :: Integer
    paidToAddress = foldl f 0 $ txInfoOutputs info
      where
        f :: Integer -> TxOut -> Integer
        f n o
            | txOutAddress o == addr = n + getLovelace (fromValue $ txOutValue o)
            | otherwise              = n
```

And the paid to address is  supposed to be the total number of lovelace paid to the specified address.So we use a fold left which is defined in the Plutus prelude, it starts with an accumulator value of zero and the idea is we just loop over all the outputs and if they go to this address,I extract the amount of lovelace contained in that output and added to the accumulator.So this here is the list of all outputs and this function now defines how to accumulate.So first argument is the previous value of the accumulator, then the output I'm focusing innow and the updated value of the accumulator.So we call this previous value n and the output o.I look at the address of that output and if it's the given address then we add to my old accumulator the number of lovelace contained in the value of this output.And otherwise we just keep the value of the old accumulator.So the effect will be as sum up all the lovelace values contained in all the outputs that go to the specified address.And that's already it, that's the logic of my validator.Now we have to compile it to Plutus core script.And this is similar to what we have seen before.So you notice that this is a typed script, so I'm not using built-in data, I'm usingHaskell types unit and script context.And we saw how to handle that for spending purposes and for minting purposes.For staking it's, for whatever reason done a bit differently.

So if we look at module Ledger.Type.Scripts, there's this function wrap stake validator.

```
type WrappedStakeValidatorType = BuiltinData -> BuiltinData -> () 
```
```
wrapStakeValidator :: UnsafeFromData r => (r -> ScriptContext -> Bool) -> WrappedStakeValidatorType
```

And provided we have a redeemer type that can be converted to built-in data and we have something of this type, which fits well to what we have defined in the example.So redeemer script context going to bool, then this wrap stake validator converts it into a function of type wrap stake validator type which is built-in data to built-in data to unit.

```haskell
stakeValidator :: Address -> StakeValidator
stakeValidator addr = mkStakeValidatorScript $
    $$(PlutusTx.compile [|| wrapStakeValidator . mkStakingValidator ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode addr
```
So using that, given an address we can use the function we just defined, apply the address to it.Then we get something of this type unit to script context to bool, which is exactly what we can pass to wrap stake validator.So the result of applying wrap stake validator to that is of type built-indata to built-in data to unit. So the whole thing together with the address is then of type address to built-in data to built-in data to unit.I compile this and we lift the given address and apply it to this compiled Plutus script so then we end up with something of the right type namely built-in data to built-in data to unit.So this is very similar to what we did with typed validators for spending orminting it's just a little bit different how to apply this wrap stake validator.And out comes a stake validator and that's all we need, so it's refreshingly short actually.

```haskell
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE TypeApplications  #-}

module Week10.Deploy
    ( writeStakeValidator
    , tryReadAddress
    ) where

import           Cardano.Api                 as API
import           Cardano.Api.Shelley         (Address (..), PlutusScript (..))
import           Cardano.Crypto.Hash.Class   (hashToBytes)
import           Cardano.Ledger.Credential   as Ledger
import           Cardano.Ledger.Crypto       (StandardCrypto)
import           Cardano.Ledger.Hashes       (ScriptHash (..))
import           Cardano.Ledger.Keys         (KeyHash (..))
import           Codec.Serialise             (serialise)
import qualified Data.ByteString.Lazy        as LBS
import qualified Data.ByteString.Short       as SBS
import           Data.Text                   (pack)
import           Plutus.V1.Ledger.Credential as Plutus
import           Plutus.V1.Ledger.Crypto     as Plutus
import           PlutusTx.Builtins           (toBuiltin)
import qualified Ledger                      as Plutus

import           Week10.Staking

writeStakeValidator :: FilePath -> Plutus.Address -> IO (Either (FileError ()) ())
writeStakeValidator file = writeFileTextEnvelope @(PlutusScript PlutusScriptV1) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . Plutus.getStakeValidator . stakeValidator

credentialLedgerToPlutus :: Ledger.Credential a StandardCrypto -> Plutus.Credential
credentialLedgerToPlutus (ScriptHashObj (ScriptHash h)) = Plutus.ScriptCredential $ Plutus.ValidatorHash $ toBuiltin $ hashToBytes h
credentialLedgerToPlutus (KeyHashObj (KeyHash h))       = Plutus.PubKeyCredential $ Plutus.PubKeyHash $ toBuiltin $ hashToBytes h

stakeReferenceLedgerToPlutus :: Ledger.StakeReference StandardCrypto -> Maybe Plutus.StakingCredential
stakeReferenceLedgerToPlutus (StakeRefBase x)                   = Just $ StakingHash $ credentialLedgerToPlutus x
stakeReferenceLedgerToPlutus (StakeRefPtr (Ptr (SlotNo x) y z)) = Just $ StakingPtr (fromIntegral x) (fromIntegral y) (fromIntegral z)
stakeReferenceLedgerToPlutus StakeRefNull                       = Nothing

tryReadAddress :: String -> Maybe Plutus.Address
tryReadAddress x = case deserialiseAddress AsAddressAny $ pack x of
    Nothing                                      -> Nothing
    Just (AddressByron _)                        -> Nothing
    Just (AddressShelley (ShelleyAddress _ p s)) -> Just Plutus.Address
        { Plutus.addressCredential        = credentialLedgerToPlutus p
        , Plutus.addressStakingCredential = stakeReferenceLedgerToPlutus s
        }
```

Now of course to use this in the cardano cli, we have to serialize the script to disk and this is very similar to what we did before.

```haskell
writeStakeValidator :: FilePath -> Plutus.Address -> IO (Either (FileError ()) ())
writeStakeValidator file = writeFileTextEnvelope @(PlutusScript PlutusScriptV1) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . Plutus.getStakeValidator . stakeValidator
```
So this write stake validator we basically just copied the function from week03 where we showed this same for spending validator for normal Plutus spending script.It's almost exactly the same, so we first apply stake validator to the address to get my stake validator and now we have to unwrap that to get to the underlying script and there's a function called get stake validator.So that was different for  spending script, but this was the only difference, the rest of this pipeline where is exactly the same that we used before.So this will write the validator to disk given the address.

And in order to conveniently do that we also need the ability to take the address in the format that the cli uses and convert it to a Plutus address and we had the same problem before in week06.So we basically just copy pasted the code we had there so these two helper functions credential ledger to Plutus and stake reference ledger to plutus and then this try read address function.So let's just copy it from week06.So given a string it passes that into a Plutus address or tries to pass it into a Plutus address.

```haskell
import System.Environment (getArgs)
import Text.Printf        (printf)
import Week10.Deploy      (tryReadAddress, writeStakeValidator)

main :: IO ()
main = do
    [file, addr'] <- getArgs
    let Just addr = tryReadAddress addr'
    printf "file: %s\naddr: %s\n" file (show addr)
    e <- writeStakeValidator file addr
    case e of
        Left err -> print err
        Right () -> printf "wrote stake validator to %s\n" file
```

Finally, we defined an  executable, which receives two command line parameters, a  file name and an address.So it passes the command line, passes these two parameters.Then passes the address, the Plutus address from this given string.And then it uses this write stake validator with the provided file and the past address to compute the stake validator parameterized by this address and serialize it to this file.And this is already all the Haskell or Plutus code that we need.So now we can try this out in the private testnet.

## Trying it on the Testnet

To try our Plutus staking script in the testnet, first lets restart the testnet and create a new user, because we want this user to receive half of all the rewards. Looking at **make-user2.sh**:

```
#!/bin/bash

vkey=tmp/user2.vkey \
skey=tmp/user2.skey \

export CARDANO_NODE_SOCKET_PATH=cardano-private-testnet-setup/private-testnet/node-bft1/node.sock
cardano-cli address key-gen \
    --verification-key-file $vkey \
    --signing-key-file $skey
cardano-cli address build \
    --testnet-magic 42 \
    --payment-verification-key-file $vkey \
    --out-file tmp/user2.addr
```

- So we just pick file names for the verification key and the signing key. 
- And then we use the cardano cli address keygen command where we just specify these two file names.So this will create a key pair and
- then we use the address build command where that command as parameter gets the payment verification key file, that's the file where we just wrote the newly created verification key  to and creates an address.Optionally, we could also  specify a starting component for this address, but we  don't do this in this case.So this is a pure payment address without a staking component.

```
[nix-shell:~/plutus-pioneer-program/code/week10]$ ./scripts/make-user2.sh
```
we can run that.And then if we look in the temp folder we see that these three files have been created.

```
[nix-shell:~/plutus-pioneer-program/code/week10]$ ls tmp

Output:
user2.addr user2.skey user2.vkey
```

we also created the script that checks the UTxOs at the address that we just created.

```
[nix-shell:~/plutus-pioneer-program/code/week10]$ ./scripts/query-utxo-user2.sh

Output:
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
```
And as expected at the moment there is no UTxO there, we just created the address so there haven't been any transactions that could possibly send any funds to that address.So the address we just created is the one we want to use to parameterize our Plutus script.And once I've done that we get the serialized script and can use that as a staking address.And then we want to register that stacking address and delegate to the one pool we have.So one step at the time, so this is the newly created address.I need one argument, a toxin to pay for the transaction, we log that, Define various file names, so this here receives the serialized Plutus script.This will receive the staking address that corresponds to the script.Then we will generate a new address where the payment path is the payment path of user1 sothe given by the verification key and sign the key of user1, but the stalking path is now our new stake address given by our script.Then find them for the registration certificate, find them for the delegation certificate.I also need the protocol parameters so file name for that and then again one file for the unsigned transaction one for the sign transaction.I set the node socket path.Now we run our executable and as arguments we give this script file that we  defined and our new address.So this will now parameterize our Plutus contract by this address and write the resulting to the score script to this file.Now that we have this script file we can take it and build the stake address from it.So there's the stake address built cardano cli command for this.Takes the magic, the location of the script file and the name of the out file.I log this new stake address.Now we want to build the new address for user1 where the...as we said, the payment component is the verification key of user1, butthe stake component is our new script.So we use the address build command testnet magic.Here the payment verification key file is the one belonging to user1, but thestake part is given by a stake script file our new script file that we generated.And we write the result to script payment address.I log it.Now we can generate the certificates, so there is cardano cli stake address  registration certificate.It just takes the name of the script file and the output file.Then we use cardano cli stake address delegation certificate, which takes again the script file.Then the stake pool id we want to delegate to, so we get that from the command we looked at earlier, the query stake pools.And write the resulting certificate to the delegation file.Now we need the protocol parameters we saw that before, there's this query protocol parameters command.And finally we can build my transaction.So takes the magic, as change address we use the new payment address for user we do that so that that address also is funded and can then accumulate rewards.Of course, we could do that in a separate transaction, but why use two transactions if we can do it in one.So we just use this new address as the change address.Out file as input the parameter we have to give to the script.This involves Plutus, because the Plutus script has to be executed to check whether the delegation is valid.So any transaction involving executing Plutus needs collateral.So as collateral can use the same input.Remember, collateral must be a pure lovelace UTxO.Okay then we attach the registration certificate and the delegation certificate.And for the delegation certificate we must also provide witnesses so that could be in the case of a normal stake address it would be the signing key belong to this.But now it's our script file and the redeemer and as redeemer remember we had type unit andI used type unit before so in order to have a serialized form of the unit value we use this unit.json which we just copied from lecture 3.And we need to provide the protocol parameters.And then we just sign so the payment input for this transaction comes from our old user1 address.So user 1 needs to sign it so we assign it with the signing key of user1 and finally we transmit.So first we look at the UTxOs of user1 because we need one of those as input for my script.So we are back to the original distribution of funds, the 450 000 ADA and the almost 450 000 ADA split between two UTxOs.So with this we now can call my script and we must provide this one of the two UTxOs as input to this script.Let's take the first one again.And it was successfully submitted.So now we have created this script-based stake address.We can see it here logged, so this is this new script stake address,that's now based on our Plutus script.I created this new payment address.Recall, the payment component is user1 but the stake component is now thePlutus script and we submitted that.So now we can also use another script we wrote, which looks at the UTxOs at this new address.And we see that there are funds there now.And that is because we use this address as a change address.So we consumed this first UTxO of the original address to pay for transaction fees and deposit fees registration fees for the certificate and the change went to this new address so we already have funds there.I also provided the script to get the stake address info.This so we see that there is some info so the delegation was successful with our new address but at the moment there are no rewards yet because recall it always takes two  or three epochs after you delegate before you first  receive rewards so we have to wait a little bit until we get rewards there.Now we waited a bit and we see that there are now 406 ADA in rewards already accumulated.Now we want to withdraw.And we created a script called withdraw user1 script which we first copy pasted from the withdrawal script we saw before we talked about Plutus and we modified it accordingly.So again it takes an argument a toxin to pay for a transaction fees.It again looks up the amount of available rewards using the script we just showed you and then picking out the appropriate field the reward account balance field as before.But now we call it amount 1 because we also have an amount 2 which is half of that.So we use this expression linux tool to do computation so we divide this amount by 2 and we addons because if this is odd the round down and then we would have too little so we need at least half.So if it's an even number  then we basically give one lovelace too much but of  course that doesn't matter.Protocol parameter file unsigned transaction signed transaction we log for debugging.Query protocol parameters so that's all very similar.So now for the transaction.So testnet magic change address we use the user1 script address this new payment address for user1.Out file as input we use the specified parameter now we also need collateral because now ourPlutus script will be executed again because it's a withdrawal action so we use the same that we use as input also as collateral.Now this is where we make sure that validation will succeed we pay the appropriate amount as amount 2 to user2.Recall, we mean that's how we parameterize our script we parameterized it by this address byuser2 address, so withdraw it will only be legal if we pay at least half of the rewards to user2.Then withdrawal as before now that's amount one.And now we as witness must provide the script file and redeemer file redeemer isagain unit and the protocol parameters.Now for signing when we did that without Plutus with a normal stake address we needed to provide two signatures one for the payment part one for the staking part we still need the one for the payment part which is again user1 signing key but now we don't need the one for the taking part because this is now replaced by here the script and the redeemer and finally we submit.But before we try that we want to show how it fails so to prove that it actually works and enforces that the specified address in this case this user2 address indeed gets paid half the rewards.So let me just for the moment delete this line so we just don't pay user2.Now let me try what happens.So as input we can use this UTxO actually we have to use that UTxO because it's the only one sitting at this address.Okay and we see it doesn't succeed and we also see that we get debugging logs insufficient reward sharing.If you recall that comes  from our trace of false in the Plutus code so the condition is not satisfied that we paid at least half  to the provided address.Now let me add that line  again, execute this again.Now the transaction was successfully submitted.If we look at the UTxO.Earlier we had a bit less than 450 000 ADA.Now we have more, 450 275.You can also check user2.Recall when we did that earlier after we created user2 there was no UTxO there.But now our transaction sent half the rewards there so we have 276 ADA there, so it works.I could withdraw but only if we satisfied the condition that user2 got at least half ofthe rewards, which is a strong indication that our Plutus script works as expected.

