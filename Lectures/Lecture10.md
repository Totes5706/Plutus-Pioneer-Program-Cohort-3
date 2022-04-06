
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

So let's do that now. So from this week's code folder I just call that script in script start private testnet.

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

- Next I look up the amount that we can withdraw. There's a peculiarity with withdrawals in Cardano, so you can only ever withdraw the whole accumulated rewards. You cannot do partial withdrawals. This means that we must know exactly how many rewards have been accumulated when we execute this command. We use the script we used before, the **query-stake-addresses-info-user1.sh** and then we use the jq tool (standard linux tool to analyze json values and extract this reward account balance field). So amount now holds the available rewards.
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

But we haven't talked about  rewarding and certifying it and those two are related  to Plutus and staking.So as I briefly mentioned before, instead of using a public private key pair to create a stake address, we can instead use a Plutus script.And then the hash of that Plutus script will give a stake address, a script stake address.And where as we saw in the previous example, if I want to withdraw my rewards for example,for a normal stake address, I have to as witness that I'm allowed to do that provide the signing key for this stake address.So if I use a script stake address, then instead the corresponding script will be executed.It will receive the redeemer and the script context and again I can use arbitrary logic to determine whether this transaction is allowed to actually withdraw those rewards.

Similarly Certifying DCert.So there are various certifications that I can attach to a transaction.In particular, important for staking out registration delegation and deregistration certificates.So if I newly create a stake address, I first have to register it by creating a transaction that contains a registration certificate for this stake address.Then if I want to delegate to a pool or change an existing validation I have to use a transaction that contains a delegation certificate.And that certificate then contains the pool I want to delegate to.And finally I can also unregister a stake address again and get the original deposit back that Ihad to pay when I registered the stake address.So for those then if I do  a delegation for example, then again the corresponding  script will be executed and can contain arbitrary logic to determine whether this delegation for  example is legal or not.


So I want to concentrate on the rewarding purpose in this lecture.

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

But we haven't looked at these two fields here.So here we have a field with all the certificates that are attached to the transaction.And now relevant for this lecture, we have a list of pairs containing of staking credentials and integers for withdrawals.So each pair corresponds to the withdrawal of rewards from the state address given by the staking credential.So the staking credential corresponds to our staking address given by a Plutus script.And the integer is the amount of lovelace we are withdrawing.So whenever we withdraw rewards from a script staking address.Then the corresponding Plutus script will be executed and we receive this credentials argument in the script purpose.I created module week 10.Staking to provide an example.And it's probably not a very useful realistic example.So the idea is that withdrawals are only allowed if at least half of the withdrawn rewards goes to a previously specified address.So anybody can withdraw rewards from the staking address.But half of the rewards always have to go to a previously specified address.So let's look how we can do this.So it's a parameterized contract, so the first argument is the parameter, theaddress, that will always receive half.Next comes the redeemer so  I just use unit and then as always for Plutus scripts, the script context.And the result is a boolean indicating whether this is okay or not.So I call the address addr, unit is always unit, context ctx.And I do a case distinction on the script context purpose, the purpose of the context.So if it's certifying, I just say true.So that means I allow arbitrary delegation and the registration.For minting and spending it's false, but the interesting case is rewarding, so rewarding credential.And here I must check that what I said is satisfied so this specified address addr receives at least half of the rewards.So if not then I log this error, a trace's error, insufficient reward sharing.And I check that this here will be the amount, the amount I'm withdrawing, the total rewards.And twice of what I pay to the specified address must be greater or equal to the total rewards which the other way around means that paid to address is at least half of the total amount.So this is the logic and now I just have to compute the various things.So as often before I define info which is the tx info field sitting in the script context.Now to get the amount, the total reward amount, given the staking credentials and those I receive here in the purpose and then I can use them as an argument here.So giving the staking credentials, I have this helper function that receive a list of pairs of staking credentials and integers.And I call that with the field we just looked at in the tx info this txinfo wdrl field, which contains such pairs.So if it's the empty list, then I didn't find the correct withdrawal and I trace an error.And if it's not empty so there is at least one pair with some credential and some amount, Check whether the credential is the one I'm interested in, the one I'm validating right now.And if so, then I have found my amount so I return that.And otherwise I recursively look at the tail of the list.So this amount function will give me the number of withdrawn lovelace.And the paid to address is  supposed to be the total number of lovelace paid to the specified address.So I use a fold left which is defined in the Plutus prelude, it starts with an accumulator value of zero and the idea is I just loop over all the outputs and if they go to this address,I extract the amount of lovelace contained in that output and added to the accumulator.So this here is the list of all outputs and this function now defines how to accumulate.So first argument is the previous value of the accumulator, then the output I'm focusing innow and the updated value of the accumulator.So I call this previous value n and the output o.I look at the address of that output and if it's the given address then I add to my old accumulator the number of lovelace contained in the value of this output.And otherwise I just keep the value of the old accumulator.So the effect will be as sum up all the lovelace values contained in all the outputs that go to the specified address.And that's already it, that's the logic of my validator.Now I have to compile it to Plutus core script.And this is similar to what we have seen before.So you notice that this is a typed script, so I'm not using built-in data, I'm usingHaskell types unit and script context.And we saw how to handle that for spending purposes and for minting purposes.For staking it's, for whatever reason done a bit differently.So if we look at module Ledger.Type.Scripts, there's this function wrap stake validator.And provided we have a redeemer type that can be converted to built-in data and we have something of this type, which fits well to what we have defined in the example.So redeemer script context going to bool, then this wrap stake validator converts it into a function of type wrap stake validator type which is built-in data to built-in data to unit.So using that, given an address I can use the function I just defined, apply the address to it.Then I get something of this type unit to script context to bool, which is exactly what I can pass to wrap stake validator.So the result of applying wrap stake validator to that is of type built-indata to built-in data to unit.So the whole thing together with the address is then of type address to built-in data to built-in data to unit.I compile this and I lift the given address and apply it to this compiled Plutus script so then I end up with something of the right type namely built-in data to built-in data to unit.So this is very similar to what we did with typed validators for spending orminting it's just a little bit different how to apply this wrap stake validator.And out comes a stake validator and that's all we need, so it's refreshingly short actually.Now of course to use this in the cardano cli, we have to serialize the script to disk and this is very similar to what I did before.So this write stake validator I basically just copied the function from week03 where I showed this same for spending validator for normal Plutus spending script.It's almost exactly the same, so I first apply stake validator to the address to get my stake validator and now I have to unwrap that to get to the underlying script and there's a function called get stake validator.So that was different for  spending script, but this was the only difference, the rest of this pipeline where is exactly the same that we used before.So this will write the validator to disk given the address.And in order to conveniently do that I also need the ability to take the address in the format that the cli uses and convert it to a Plutus address and we had the same problem before in week06.So I basically just copy pasted the code we had there so these two helper functions credential ledger to Plutus and stake reference ledger to plutus and then this try read address function.So let's just copy it from week06.So given a string it passes that into a Plutus address or tries to pass it into a Plutus address.Finally, I defined an  executable, which receives two command line parameters, a  file name and an address.So it passes the command line, passes these two parameters.Then passes the address, the Plutus address from this given string.And then it uses this write stake validator with the provided file and the past address to compute the stake validator parameterized by this address and serialize it to this file.And this is already all the Haskell or Plutus code that we need.So now we can try this out in the private testnet.
