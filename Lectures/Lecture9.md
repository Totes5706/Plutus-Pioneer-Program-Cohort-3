Plutus Pioneer Program - Cohort 3 
March 17th, 2022

Contributed By:
[Joe Totes](https://github.com/Totes5706)


Offical Video by Lars Brünjes: [PPP-Cohort3-Lecture9](https://youtube.com/playlist?list=PLNEK_Ejlx3x2zSFnzWA4Gbr_AVTz-4rzf)



# Lecture 9: Marlowe

## Table of Contents

- [Lecture 9: Marlowe](#lecture-9-marlowe)
  - [Table of Contents](#table-of-contents)
  - [Preparation for Lecture 9](#preparation-for-lecture-9)
  - [Introduction](#introduction)
  - [Simon Thompson: Marlowe Overview](#simon-thompson-marlowe-overview)
  - [Alexander Nemash: Marlowe in Plutus](#alexander-nemash-marlowe-in-plutus)
  - [Brian Bush: The Marlowe CLI](#brian-bush-the-marlowe-cli)
  - [Marlowe Playground Demo](#marlowe-playground-demo)
  - [Homework](#homework)

## Preparation for Lecture 8

This week we will be learning how to use Marlowe in the browser. 

We can get started by visting:
[https://marlowe-playground-staging.plutus.aws.iohkdev.io/#/](https://marlowe-playground-staging.plutus.aws.iohkdev.io/#/)

We can now begin with the lecture.


## Introduction

Welcome to the second to last lecture of the Plutus Pioneer Program. This lecture is not 
about Plutus, but about Marlowe; which is a domain specific language for financial contracts built on top of Plutus. 

Marlowe has also changed a little bit since the recording of this lecture for the first iteration.
It was decided to not edit this lecture as much, because it is still mostly accurate and in particular the demo at the end still works.

In the previous lectures we have learnt about all the important ingredients for writing a Plutus application.

We have first looked at the extended UTxO model - the accounting model that Cardano uses - and the additions that Plutus brings to it.

Then we have talked about on-chain validation, minting policies, writing off-chain code, we have seen how to deploy smart contracts and also how to test them.

Plutus is a very powerful language. So powerful, in fact, that you can implement other languages on top of it - you can write an interpreter in Plutus for other languages.

One such language is Marlowe. Marlowe is a Domain Specific Language (DSL) for smart contracts.

For this lecture, Professor Simon Thompson, a very prominent figure in the Haskell community who leads the Marlowe team, and his colleague Alex Nemish will give guest lectures to tell us a bit about Marlowe.

In contrast  to previous iterations of this course it is now  actually possible to deploy Marlowe contracts on  the blockchain, on the Mainnet or on the Testnet.  
There is a very nice tool to do this, it is called the Marlowe-CLI, which is similar to  the Cardano-CLI. 
So in this lecture we will also  include a presentation by our colleague Brian bush, who will talk about Marlowe -CLI and explain how it works. 

Afterwards we will look at the Marlowe playground and play with a simple smart contract.

## Simon Thompson: Marlowe Overview

