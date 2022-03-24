
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

## Preparation for Lecture 9

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

### What does a financial contract do?

Let's start by looking at what a financial contract can do.

![Screenshot 2022-03-24 at 08-38-27 PPP 030902 - Simon Thompson Marlowe Overview](https://user-images.githubusercontent.com/59018247/159917887-2d50f403-379e-4d5c-862c-5db631bff116.png)

A contract can accept payments from participants in the contract.

![Screenshot 2022-03-24 at 08-39-13 PPP 030902 - Simon Thompson Marlowe Overview](https://user-images.githubusercontent.com/59018247/159918025-0b5cd830-7144-462e-8a8f-9d127c2d981c.png)


Depending on choices made by one the participants, it can evolve in different directions.

![Screenshot 2022-03-24 at 08-39-47 PPP 030902 - Simon Thompson Marlowe Overview](https://user-images.githubusercontent.com/59018247/159918096-9e096f69-06da-4c99-adef-bcb23ad03d11.png)

It can make decisions based on external information such as the information coming from a stock exchange. So, information coming from an oracle can determine the future behaviour of a contract.

![Screenshot 2022-03-24 at 08-40-37 PPP 030902 - Simon Thompson Marlowe Overview](https://user-images.githubusercontent.com/59018247/159918218-2d123f1a-3138-46e1-9d08-da76ea976634.png)

A contract can also make payments out. If money has been deposited in the contract, that money can be deposited out to participants.

![Screenshot 2022-03-24 at 08-41-45 PPP 030902 - Simon Thompson Marlowe Overview](https://user-images.githubusercontent.com/59018247/159918420-32d34fc0-6ccc-4a28-96df-2282c9c1eee8.png)

So we have flows of money and choices according to external factors.

One final thing that we have is that the roles in a contract are things that themselves can be owned. We represent that in Marlowe by minting tokens that represent those roles. That means that we can use those tokens as evidence that somebody is meant to be playing a role. They are a form of security that a person submitting a transaction is allowed to submit that transaction, but also it means that these roles are tradable. A role could be traded by another person or another contract.

![Screenshot 2022-03-24 at 08-44-30 PPP 030902 - Simon Thompson Marlowe Overview](https://user-images.githubusercontent.com/59018247/159918885-d963f50b-4960-4d81-bb33-6771b14ccf09.png)

### Design

Now let's think about how to design a language based on these ingredients.

![Screenshot 2022-03-24 at 08-45-27 PPP 030902 - Simon Thompson Marlowe Overview](https://user-images.githubusercontent.com/59018247/159919037-98a45de5-15a6-4975-9290-c6ebd62728e3.png)

When we design a language of contracts, what we are really doing is designing a programming language. A smart contract is just a program running on a blockchain.

A contract could, in principle, run forever. And also, more subtly, it could get stuck waiting for an input forever.

It could terminate while holding assets, locking them up forever.

So there's a whole lot of security issues that a program might have.

![Screenshot 2022-03-24 at 08-48-13 Plutus Pioneer Program - Iteration #3 - Lecture #9](https://user-images.githubusercontent.com/59018247/159919605-d43745ec-fbda-444e-b8d3-1e2b9b5acc34.png)


### Designed for safety

What we chose to do was to design for safety.
Contracts are finite

Firstly, contracts are designed to be finite. Their life will be finite, there is no recursion or looping in Marlowe. We will come back to that a bit later on when we talk about Marlowe being embedded in other languages.

### Contracts will terminate

We can be sure that contracts will terminate. We do that by putting timeouts on every external action. Every choice, every deposit of money into the contract comes with a deadline. Marlowe contracts cannot wait forever for somebody to make a choice or for an action to happen. If you hit the timeout then an alternative course is taken.

### No assets retained on close

We've designed the semantics of the language so that when a contract reaches its close, at the end of its lifetime, any money left in the contract will be refunded to participants.

### Conservation of value

Conservation of value is something that we get for free from the underlying blockchain. The blockchain guarantees that we can't double spend and because we are using the transaction mechanisms of the underlying blockchain, we can be sure that we are getting conservation of value.

So this is giving us a lot of guarantees out of the box. These are not guarantees that you get from Plutus contracts in general. A Plutus contract could go on forever, it need not terminate and it could terminate while holding a whole collection of assets which then become unreachable.


![Screenshot 2022-03-24 at 08-51-36 Plutus Pioneer Program - Iteration #3 - Lecture #9](https://user-images.githubusercontent.com/59018247/159920158-29260325-e294-47a1-b6bc-8d9a03056ddd.png)

### The Marlowe Language

So what does the language look like? Let's cut to the chase.

Marlowe, at heart is represented as a Haskell datatype.

```haskell
data Contract = Close
| Pay Party Payee Value Contract
| If Observation Contract Contract
| When [Case Action Contract] Timeout Contract
| Let ValueId Value Contract
| Assert Observation Contract
deriving (Eq,Ord,Show,Read,Generic,Pretty)
```

We have a ```Pay``` construct. In that a Party in the contract makes a payment to a Payee of a particular Value, and then the contract continues with what we call the continuation contract.

```haskell
Pay Party Payee Value Contract
```

We can go in two separate directions. We can observe ```If``` a particular Observation is true or not. If the observation is true we follow the first Contract, if it is false we follow the second Contract.

```haskell
If Observation Contract Contract
```

The most complex construct in Marlowe is the ```When``` construct. It takes three arguments. The first of those is a list of Contract/Action pairs - a list of Cases.

```haskell
When [Case Action Contract] Timeout Contract
```

What the When construct does is wait for one of a number of Actions. When one of those Actions happens, it performs the corresponding Contract. For example, it could be waiting for a deposit. If we have a case where the first part of the pair is a deposit, then we execute the corresponding second part of the pair. Similarly with making a choice or with getting a value from an oracle.

Here we are waiting for external actions and, of course, the contract can\'t make those actions happen. A contract can\'t force somebody to make a choice. It can't force somebody to make a deposit. But what we can do is say that if none of these actions takes place then we will hit the Timeout, and when we hit the Timeout, we will perform the Contract represented by the final argument to the When construct.

So, we can guarantee that something will happen in the When construct, either by one of the actions triggering a successive contract, or we hit the timeout and go to that continuation.

Finally we have the ```Close``` construct which has the semantics defined so that nothing is retained when we close.

```haskell
data Contract = Close
```

That is the Marlowe language, and we will see that we can use these to construct Marlowe contracts in a variety of ways.


![Screenshot 2022-03-24 at 08-56-53 Plutus Pioneer Program - Iteration #3 - Lecture #9](https://user-images.githubusercontent.com/59018247/159921044-8f6d48e3-21e9-40ba-9987-8bcc8870cc7d.png)

### The Marlowe Product

So that is the language. What is the Marlowe product itself?

We have a suite of things. First we'll look at the overall vision for Marlowe and then look at where we are in terms of fulfilling that vision.


![Screenshot 2022-03-24 at 09-57-42 Plutus Pioneer Program - Iteration #3 - Lecture #9](https://user-images.githubusercontent.com/59018247/159932268-252fad81-3566-4275-b292-7f8546c12d41.png)

We have a prototype for Marlowe Run. That is the system through which an end user will interact with contracts running on the Cardano blockchain. You can think of Marlowe Run as the Marlowe DAPP. It's the things that allows Marlowe contracts to be executed.

We're also building a market where contracts can be uploaded, downloaded, and where we can provide various kinds of assurance about those contracts.

We allow contracts to be simulated interactively and we call that Marlowe Play. We allow contracts to be built in various different ways and we call that Marlowe Build. In fact fact what we've done at the moment is bundle those two - Marlowe Play and Build - into what we call the Marlowe Playground.

So as things stand at the moment you can use the Marlowe Playground to simulate and construct Marlowe contracts we're in the process of redesigning the user experience based on what we've done with Marlowe Run.

This is the prototype of how end users will interact with Marlowe on the blockchain. Our intention is that we'll have all these products available running on the Cardano blockchain when we have the full support for this which will involve having the Plutus Application Backend and the wallet back end and so on working as they should.

![Screenshot 2022-03-24 at 10-00-13 Plutus Pioneer Program - Iteration #3 - Lecture #9](https://user-images.githubusercontent.com/59018247/159932867-bb12f7f0-6012-4d2f-8a93-6764bc960bc6.png)

### Marlowe Run Demo

We'll now look at a demo of what we have in Marlowe Run to give you a sense of what we can do at the moment in terms of giving users the experience that they will have when Marlowe is running on blockchain. This will be the app that is going to provide that experience.

You can find the Marlowe Playground at

[https://run.marlowe-finance.io/](https://run.marlowe-finance.io/)


![Screenshot 2022-03-24 at 10-01-38 https __run marlowe-finance io](https://user-images.githubusercontent.com/59018247/159933110-83085082-c99f-477a-ac8d-3761afa59d58.png)

















