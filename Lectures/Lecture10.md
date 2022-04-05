
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
totinj@penguin:~/plutus-pioneer-program$ git pull --recurse-submodules
```
Head into he week10 subfolder and open the cabal.project file:

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

As I mentioned before, a lot of things are already automatically created for us, in particular in this folder cardano private testnet setup private testnet addresses are various artifacts.

```
[nix-shell:~/plutus-pioneer-program/code/week10]$ ls cardano-private-testnet-setup/private-testnet/addresses/


Output:
pool-owner1-stake.addr      pool-owner1-stake.vkey  pool-owner1.vkey        user1-stake.reg.cert  user1.addr
pool-owner1-stake.reg.cert  pool-owner1.addr        user1-stake.addr        user1-stake.skey      user1.skey
pool-owner1-stake.skey      pool-owner1.skey        user1-stake.deleg.cert  user1-stake.vkey      user1.vkey

```

But in particular we see a user1 has been created with verification key, signing key, payment address, staking key pair, verification key, signing key, corresponding staking address.If you recall from last time, we talked about the cardano cli, there's a very useful query command, query UTxO to get the UTxO at an address.

```
#!/bin/bash
export CARDANO_NODE_SOCKET_PATH=cardano-private-testnet-setup/private-testnet/node-bft1/node.sock
cardano-cli query utxo \
    --testnet-magic 42 \
    --address $(cat cardano-private-testnet-setup/private-testnet/addresses/user1.addr)
```

So using again the correct node socket path and the correct testnet magic and taking the address that I just showed you, so user1 address, I have this script.

```
[nix-shell:~/plutus-pioneer-program/code/week10]$ ./scripts/query-utxo-user1.sh

Output:
                           TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------
2207d6294a2a7b8ba664c85f18bed9d49d5837240de52547df0b101762a2a4a7     0        450000000000 lovelace + TxOutDatumNone
2207d6294a2a7b8ba664c85f18bed9d49d5837240de52547df0b101762a2a4a7     1        449999000000 lovelace + TxOutDatumNone
```


And if I execute it, I see that conveniently this user already has lots of funds.So if we count zeros, so we have 450 000 ADA and this UTxO and almost 450 000ADA in that UTxO, so almost 900 000 ADA.Another query command the cardano cli provides is query stake pools, which is the name suggests lists all stake pools.So if we execute that, we see we have one stake pool already and this is its stake pool id.There's another query command, stake address info.Which takes a stake address and then gives us information about that stake address.So as we saw, the user1 stake address has been created for us.Executing that script, gives us the following information that this stake address delegates to a pool, which of course, is the only pool there is.This is the stake address in question and we see that we already have quite a lot of accumulated rewards.So if I count digits correctly it's at the moment 2189 ADA.So how can we withdraw those rewards?So I wrote a script for that, which will create a appropriate transaction and thatscript takes one argument, a UTxO as input.As we saw there are two,  we can pick any of the two.So this is the argument.Next I look up the amount that we can 
