
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
  - [Homework](#homework)
  
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

So the repository we including into our course repository via git submodule is woof pool's cardano private testnet setup.There is nice collection of scripts that make it extremely easy to quickly run your own private testnet. So there are lots of options and very nice explanations but we are just using the default setup. This will run three nodes, one stake pool, and creates one user.

In this week's code folder, there is a scripts subfolder with lots of scripts and one of the scripts is called **start-private-testnet.sh**.


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

