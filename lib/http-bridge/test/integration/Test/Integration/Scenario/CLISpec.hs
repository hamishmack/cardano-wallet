{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Test.Integration.Scenario.CLISpec
    ( specNoCluster
    , specWithCluster
    ) where

import Prelude

import Cardano.Wallet.Api.Types
    ( ApiAddress, ApiWallet )
import Control.Monad
    ( forM_ )
import Data.List
    ( length )
import Data.Proxy
    ( Proxy (..) )
import Data.Text
    ( Text )
import System.Command
    ( Exit (..), Stderr (..), Stdout (..) )
import System.Exit
    ( ExitCode (..) )
import Test.Hspec
    ( SpecWith, describe, it )
import Test.Hspec.Expectations.Lifted
    ( shouldBe, shouldContain, shouldNotContain )
import Test.Integration.Framework.DSL
    ( Context (..)
    , Headers (..)
    , Payload (..)
    , cardanoWalletCLI
    , cardanoWalletLauncherCLI
    , createWalletViaCLI
    , deleteWalletViaCLI
    , expectResponseCode
    , expectValidJSON
    , generateMnemonicsViaCLI
    , getFromResponse
    , getWalletViaCLI
    , json
    , listWalletsViaCLI
    , request
    , updateWalletViaCLI
    , walletId
    )
import Test.Integration.Framework.TestData
    ( falseWalletIds, mnemonics15, mnemonics18 )

import qualified Data.Text as T
import qualified Network.HTTP.Types.Status as HTTP

version :: Text
version = "2019.5.8"

specNoCluster :: SpecWith ()
specNoCluster = do

    it "CLI - Shows version" $  do
        (Exit c, Stdout out) <- cardanoWalletCLI ["--version"]
        let v = T.dropWhileEnd (== '\n') (T.pack out)
        v `shouldBe` version
        c `shouldBe` ExitSuccess

    it "CLI - cardano-wallet-launcher shows help on bad argument" $  do
        (Exit c, Stdout out) <- cardanoWalletLauncherCLI ["--bad arg"]
        out `shouldContain` "cardano-wallet-launcher"
        c `shouldBe` ExitFailure 1

    describe "CLI - cardano-wallet-launcher shows help with" $  do
        let test option = it option $ do
                (Exit c, Stdout out) <- cardanoWalletLauncherCLI [option]
                out `shouldContain` "cardano-wallet-launcher"
                c `shouldBe` ExitSuccess
        forM_ ["-h", "--help"] test

    it "CLI - cardano-wallet shows help on bad argument" $  do
        (Exit c, Stdout out) <- cardanoWalletCLI ["--bad arg"]
        out `shouldContain` "Cardano Wallet CLI"
        c `shouldBe` ExitFailure 1

    describe "CLI - cardano-wallet shows help with" $  do
        let test option = it option $ do
                (Exit c, Stdout out) <- cardanoWalletCLI [option]
                out `shouldContain` "Cardano Wallet CLI"
                c `shouldBe` ExitSuccess
        forM_ ["-h", "--help"] test

    it "CLI - Can generate mnemonics with default size" $  do
        (Exit c, Stdout out) <- generateMnemonicsViaCLI []
        length (words out) `shouldBe` 15
        c `shouldBe` ExitSuccess

    describe "CLI - Can generate mnemonics with different sizes" $ do
        let test size = it ("--size=" <> show size) $ do
                (Exit c, Stdout out) <- generateMnemonicsViaCLI ["--size", show size]
                length (words out) `shouldBe` size
                c `shouldBe` ExitSuccess
        forM_ [9, 12, 15, 18, 21, 24] test

    describe "CLI - It can't generate mnemonics with an invalid size" $ do
        let sizes = ["15.5", "3", "6", "14", "abc", "👌", "0", "~!@#%" , "-1000"
                , "1000"]
        forM_ sizes $ \(size) -> it ("--size=" <> size) $ do
            (Exit c, Stdout out, Stderr err) <- generateMnemonicsViaCLI ["--size", size]
            c `shouldBe` ExitFailure 1
            err `shouldBe` "Invalid mnemonic size. Expected one of:\
            \ 9,12,15,18,21,24\n"
            out `shouldBe` mempty

specWithCluster :: SpecWith Context
specWithCluster = do

    describe "CLI1 - Can create wallet with different mnemonic sizes" $ do
        forM_ ["15", "18", "21", "24"] $ \(size) -> it size $ \_ -> do
            let walletName = "Wallet created via CLI"
            Stdout mnemonics <- generateMnemonicsViaCLI ["--size", size]
            let passphrase = "Secure passphrase"

            c <- createWalletViaCLI [walletName] mnemonics "\n" passphrase
            c `shouldBe` ExitSuccess

            Stdout outList <- listWalletsViaCLI
            outList `shouldContain` walletName

    describe "CLI1 - Can create wallet with different mnemonic snd factor sizes" $ do
        forM_ ["9", "12"] $ \(size) -> it size $ \_ -> do
            let walletName = "Wallet created via CLI"
            Stdout mnemonics <- generateMnemonicsViaCLI []
            Stdout mnemonics2 <- generateMnemonicsViaCLI ["--size", size]
            let passphrase = "Secure passphrase"

            c <- createWalletViaCLI [walletName] mnemonics mnemonics2 passphrase
            c `shouldBe` ExitSuccess

            Stdout outList <- listWalletsViaCLI
            outList `shouldContain` walletName

    describe "CLI1 - Can't create wallet with wrong size of mnemonic" $ do
        forM_ ["9", "12"] $ \(size) -> it size $ \_ -> do
            let walletName = "Wallet created via CLI"
            Stdout mnemonics <- generateMnemonicsViaCLI ["--size", size]
            Stdout mnemonics2 <- generateMnemonicsViaCLI ["--size", size]
            let passphrase = "Secure passphrase"

            c <- createWalletViaCLI [walletName] mnemonics mnemonics2 passphrase
            c `shouldBe` (ExitFailure 1)

            Stdout outList <- listWalletsViaCLI
            outList `shouldNotContain` walletName

    describe "CLI1 - Can't create wallet with wrong size of mnemonic snd factor" $ do
        forM_ ["15", "18", "21", "24"] $ \(size) -> it size $ \_ -> do
            let walletName = "Wallet created via CLI"
            Stdout mnemonics <- generateMnemonicsViaCLI ["--size", size]
            Stdout mnemonics2 <- generateMnemonicsViaCLI ["--size", size]
            let passphrase = "Secure passphrase"

            c <- createWalletViaCLI [walletName] mnemonics mnemonics2 passphrase
            c `shouldBe` (ExitFailure 1)

            Stdout outList <- listWalletsViaCLI
            outList `shouldNotContain` walletName

    it "CLI - Can get a wallet" $ \ctx -> do
        walId <- createWallet ctx "1st CLI Wallet" mnemonics15
        (Exit c, Stdout out, Stderr err) <- getWalletViaCLI walId
        err `shouldBe` "Ok.\n"
        expectValidJSON (Proxy @ApiWallet) out
        out `shouldContain` "1st CLI Wallet"
        c `shouldBe` ExitSuccess

    describe "CLI - Cannot get wallets with false ids" $ do
        forM_ falseWalletIds $ \(title, walId) -> it title $ \_ -> do
            (Exit c, Stdout out, Stderr err) <- getWalletViaCLI walId
            out `shouldBe` ""
            if (title == "40 chars hex") then
                err `shouldBe` "Wallet not found.\n"
            else
                err `shouldBe` "wallet id should be an hex-encoded string of\
                    \ 40 characters\n"

            if (title == "40 chars hex") then
                c `shouldBe` ExitSuccess
            else
                c `shouldBe` ExitFailure 1


    it "CLI - Can list wallets" $ \ctx -> do
        _ <- createWallet ctx "1st CLI Wallet" mnemonics15
        _ <- createWallet ctx "2nd CLI Wallet" mnemonics18
        (Exit c, Stdout out, Stderr err) <- listWalletsViaCLI
        err `shouldBe` "Ok.\n"
        expectValidJSON (Proxy @[ApiWallet]) out
        out `shouldContain` "1st CLI Wallet"
        out `shouldContain` "2nd CLI Wallet"
        c `shouldBe` ExitSuccess

    it "CLI - Can update wallet name" $ \ctx -> do
        walId <- createWallet ctx "1st CLI Wallet" mnemonics15
        let args = [walId, "--name", "new name"]
        (Exit c, Stdout out, Stderr err) <- updateWalletViaCLI args
        err `shouldBe` "Ok.\n"
        expectValidJSON (Proxy @ApiWallet) out
        out `shouldContain` "new name"
        c `shouldBe` ExitSuccess

    it "CLI - Can delete wallet" $ \ctx -> do
        walId <- createWallet ctx "CLI Wallet" mnemonics15
        (Exit c, Stdout out, Stderr err) <- deleteWalletViaCLI walId
        err `shouldBe` "Ok.\n"
        out `shouldNotContain` "CLI Wallet"
        c `shouldBe` ExitSuccess

    it "CLI - Can list addresses" $ \ctx -> do
        walId <- createWallet ctx "CLI Wallet" mnemonics15
        (Exit c, Stdout out, Stderr err) <- command [] "cardano-wallet"
            ["address", "list", "--port", "1337", walId]
        err `shouldBe` "Ok.\n"
        expectValidJSON (Proxy @[ApiAddress]) out
        c `shouldBe` ExitSuccess
  where

    createWallet :: Context -> Text -> [Text] -> IO String
    createWallet ctx name mnemonics = do
       let payload = Json [json| {
               "name": #{name},
               "mnemonic_sentence": #{mnemonics},
               "passphrase": "Secure Passphrase"
               } |]
       r <- request @ApiWallet ctx ("POST", "v2/wallets") Default payload
       expectResponseCode @IO HTTP.status202 r
       return (T.unpack $ getFromResponse walletId r)
