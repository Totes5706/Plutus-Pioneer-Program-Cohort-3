
# Lecture 9: Marlowe

Plutus Pioneer Program - Cohort 3 
March 17th, 2022

Contributed By:
[Joe Totes](https://github.com/Totes5706)


Offical Video by Lars Brünjes: [PPP-Cohort3-Lecture9](https://youtube.com/playlist?list=PLNEK_Ejlx3x2zSFnzWA4Gbr_AVTz-4rzf)


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

![Screenshot 2022-03-24 at 08-31-01 PPP 030902 - Simon Thompson Marlowe Overview](https://user-images.githubusercontent.com/59018247/159916706-2cac5426-b67c-4969-a9e1-e8e2d3474131.png)

![Screenshot 2022-03-24 at 08-31-52 PPP 030902 - Simon Thompson Marlowe Overview](https://user-images.githubusercontent.com/59018247/159916827-2dd81e8d-4f93-42cc-930e-100ecc9d8a5d.png)

Marlowe is a special-purpose language for writing financial contracts on Cardano.

### Why do we build DSLs?

One reason is that we want to build languages that are closer to the language of the user and not so much the language of the system. They are designed to be in the specific domain of the application. A financial language will talk about payments, for example.

When we write a DSL, we get some advantages. We can write down things in that domain, but we can't perhaps write as much as we could in a general purpose language. And, if we do work in this more specialised context, we have the advantage of being able to give people better feedback and better error messages. We can also give more guarantees on program behaviour. That's one of the things that will be stressed in this lecture.


![Screenshot 2022-03-24 at 08-34-11 PPP 030902 - Simon Thompson Marlowe Overview](https://user-images.githubusercontent.com/59018247/159917160-4312cec9-813f-4087-b306-cd421baaeaa5.png)

### What kind of assurance can we give?

We can give two kinds of assurance. We can make sure that contracts do what they are supposed to do, but we can also make sure that they don't do what they shouldn't. We will see both aspects of that as we go along.

We've designed the language to be a simple as possible and the implementation reflects that, and we'll talk a bit about that later on. Contracts are nice and readable, and also we can easily simulate them, so we can present to users a very clear picture of how their contract in Marlowe will behave.

In fact, we can do more than that. Because they are particularly restricted, we can explore every possible behavior path that a contract can take, before it is executed. So, we can give complete guarantees about how a contract will behave, not just on one or two tests, but on every possibly execution sequence.

It's also more straightforward to write mathematical proofs of various kinds of safety, so that is the strongest criteria that we can hit in this kind of world; a mathematical proof that the system will do certain things and won't do others.


![Screenshot 2022-03-24 at 08-36-27 PPP 030902 - Simon Thompson Marlowe Overview](https://user-images.githubusercontent.com/59018247/159917538-75816d4c-1744-4924-a834-87e98743cc33.png)

Let's start by looking at what a financial contract can do.

A contract can accept payments from participants in the contract.

Depending on choices made by one the participants, it can evolve in different directions.








































