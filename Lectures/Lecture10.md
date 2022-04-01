
# Lecture 10: Staking and the Private Testnet

Plutus Pioneer Program - Cohort 3 
March 30th, 2022

Contributed By:
[Joe Totes](https://github.com/Totes5706)


Offical Video by Lars Brünjes: [PPP-Cohort3-Lecture9](https://youtube.com/playlist?list=PLNEK_Ejlx3x3EV7FKhlogJgS27dWgwI9B)


## Table of Contents

- [Lecture 9: Marlowe](#lecture-9-marlowe)
  - [Table of Contents](#table-of-contents)
  - [Preparation for Lecture 10](#preparation-for-lecture-10)
  - [Introduction](#introduction)
  - [The Private Testnet](#the-private-testnet)
  - [Plutus and Staking](#plutus-and-staking)
  - [Trying it on the Testnet](#trying-it-on-the-testnet)
  - [Conclusion](#conclusion)
  - [Homework](#homework)
  
## Preparation for Lecture 10

## Introduction

In previous iterations of this course, in the tenth lecture, we did another walkthrough where we showed how to create a smart contract, test it, write on-chain and off-chain code and then finally deploy it with the PAB. However, that was before the Alonzo hard fork, so we could not try things on the real blockchain. We instead just used the PAB simulator.

Now after Alonzo, it would be much nicer to show something on the real blockchain. Unfortunately, the PAB is still not in a state where this is easily doable. We have already done something similar in lecture six where we showed how to use the PAB to mint a token on the testnet.

Some of you asked about staking in Plutus, so we decided to do that instead which was not really possible before the Alonzo hard fork. All the mechanisms were there already, however there was no way to try it out because all we had were the plutus playground and the emulator. Neither of these  have a concept of staking. This is now of course different because after Alonzo, we have the testnet or the mainnet available. 

The second problem is that on the testnet and the mainnet things take quite a lot of time. If you, for example, delegate to a stake pool then it takes many days before you receive your first rewards. Also because an epoch on the mainnet and on the testnet lasts five days, it would take a couple weeks to see results. For obvious reasons we didn't think that that was suitable for this lecture.

Luckily, there is also the option to run a private testnet. The advantage of doing that is that you can use different parameters from the mainnet. So in particular, you can make epochs much shorter, so that it is much easier to try something staking related because you do not have to wait for five days before anything happens. So in this lecture we will do that; we will take a very simple example, how to use Plutus in relation to staking and demonstrate it using a private testnet with much shorter epochs.
