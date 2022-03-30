# How to build your own Plutus Project on Cardano

Head to the plutus-start github to get started:

[https://github.com/input-output-hk/plutus-starter](https://github.com/input-output-hk/plutus-starter)

![Screenshot 2022-03-29 at 16-12-42 input-output-hk_plutus-starter A starter project for Plutus apps](https://user-images.githubusercontent.com/59018247/160698741-00a59420-e4a1-40b5-b112-024836ee8695.png)
## Click on the green "Use this template" button in the top right corner of the page to clone the template into your own personal repo:


![Screenshot 2022-03-29 at 16-12-12 input-output-hk_plutus-starter A starter project for Plutus apps](https://user-images.githubusercontent.com/59018247/160698703-a983ccbc-7f6f-4acf-ab11-1cb615556430.png)


## Select a name for your new Cardano project/repo:

![Screenshot 2022-03-29 at 16-13-25 Build software better together](https://user-images.githubusercontent.com/59018247/160698849-2758b939-63df-40e5-ba17-af1b392789ea.png)

## Remove the examples and pab directories, they will be replaced with out project files

![Screenshot 2022-03-29 at 21-57-02 input-output-hk_plutus-starter A starter project for Plutus apps](https://user-images.githubusercontent.com/59018247/160735287-c8d71878-b634-4e4e-bbfd-72f0fbbe0453.png)

![Screenshot 2022-03-29 at 21-57-28 input-output-hk_plutus-starter A starter project for Plutus apps](https://user-images.githubusercontent.com/59018247/160735333-c449ea3e-8ce7-43e2-a2de-51e56d151387.png)

## Open the plutus-starter.cabal file, we can start by renaming this to our project:

![Screenshot 2022-03-29 at 21-59-07 input-output-hk_plutus-starter A starter project for Plutus apps](https://user-images.githubusercontent.com/59018247/160735487-193dc04b-510b-45a0-86a3-7a5aa6a7b746.png)

## Change the name to your project name at the top

![Screenshot 2022-03-29 at 21-59-48 Editing NFT-Maker_NFT-Maker cabal at main · Totes5706_NFT-Maker](https://user-images.githubusercontent.com/59018247/160735578-3479ce69-a92a-4ad6-8c8f-b2be2e0085b7.png)

## Change the name, the author and the maintainer in this file: 

![Screenshot 2022-03-29 at 22-01-46 input-output-hk_plutus-starter A starter project for Plutus apps](https://user-images.githubusercontent.com/59018247/160735767-c992dae8-4d60-42a2-a915-658ef85d83ec.png)

Example:

```haskell
cabal-version:      2.4
name:               NFT-Maker
version:            0.1.0.0

-- A short (one-line) description of the package.
-- synopsis:

-- A longer description of the package.
-- description:

-- A URL where users can report bugs.
-- bug-reports:

license: Apache-2.0
license-files: LICENSE
author:             Joe Totes
maintainer:         totinj@gmail.com
...
```
## Scroll to the bottom, we will be editing the Haskell files being linked here: 

![Screenshot 2022-03-29 at 22-08-39 input-output-hk_plutus-starter A starter project for Plutus apps](https://user-images.githubusercontent.com/59018247/160736548-57c98335-c1fb-4129-a970-f24251e64e21.png)

## Example with comments:

```haskell

library
  {- Include this to import the ghc-options and language above -}
  import: lang
  
  {- This is the directory location where our onchain code files will go, and also a utility function for my project -}
  hs-source-dirs:      src     

  {- These are the file names that will get called by our executable files listed below -}
  exposed-modules:     Token.OnChain
                       Utils     
                       
  {- These are the imports needed for the files Token.Onchain and Utils. Always include  base >= 4.9 && < 5 for Haskell. -}                     
  build-depends:       base >= 4.9 && < 5
                     , aeson
                     , bytestring
                     , cardano-api
                     , containers
                     , data-default
                     , freer-extras
                     , openapi3
                     , playground-common
                     , plutus-contract
                     , plutus-ledger
                     , plutus-ledger-api
                     , plutus-tx-plugin
                     , plutus-tx
                     , plutus-use-cases
                     , serialise
                     , text

{- This is the first executable file that will get called from the cabal exec command, and will be the main :: IO () files for our project -} 
executable token-policy

{- Include this to import the ghc-options and language above -}
  import: lang
  
{- This is the file name token-policy.hs, one of the executable files -}
  main-is: token-policy.hs
  
{- This is the local directory location of token-policy.hs -} 
  hs-source-dirs:      app

{- Always include this -} 
  ghc-options:         -threaded
   
{- My executable files have no imports, therefore I only need to include base >= 4.9 && < 5, and the project name  -}  
  build-depends:       base >= 4.9 && < 5
                     , NFT-Maker


{- This is the second executable file that will get called from the cabal exec command, and will be the main :: IO () files for our project -} 
executable token-name

{- Include this to import the ghc-options and language above -}
  import: lang

{- This is the file name token-name.hs, a second executable file -}
  main-is: token-name.hs
  
{- This is the local directory location of token-name.hs -}   
  hs-source-dirs:      app
  
{- Always include this -} 
  ghc-options:         -threaded
  
{- My executable files have no imports, therefore I only need to include base >= 4.9 && < 5, and the project name  -} 
  build-depends:       base >= 4.9 && < 5
                    , NFT-Maker
```

## Example without comments

```haskell
library
  import: lang
  hs-source-dirs:      src
  exposed-modules:     Token.OnChain
                       Utils                   
  build-depends:       base >= 4.9 && < 5
                     , aeson
                     , bytestring
                     , cardano-api
                     , containers
                     , data-default
                     , freer-extras
                     , openapi3
                     , playground-common
                     , plutus-contract
                     , plutus-ledger
                     , plutus-ledger-api
                     , plutus-tx-plugin
                     , plutus-tx
                     , plutus-use-cases
                     , serialise
                     , text

executable token-policy
  import: lang
  main-is: token-policy.hs
  hs-source-dirs:      app
  ghc-options:         -threaded
  build-depends:       base >= 4.9 && < 5
                     , NFT-Maker

executable token-name
  import: lang
  main-is: token-name.hs
  hs-source-dirs:      app
  ghc-options:         -threaded
  build-depends:       base >= 4.9 && < 5
                     , NFT-Maker
```
