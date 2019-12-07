{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Prelude

import Cardano.BM.Data.Severity
    ( Severity (..) )
import Cardano.BM.Trace
    ( Trace, logInfo )
import Cardano.CLI
    ( withLogging )
import Cardano.Faucet
    ( initFaucet, mkFeeEstimator, sockAddrPort )
import Cardano.Launcher
    ( ProcessHasExited (..), withUtf8Encoding )
import Cardano.Wallet.Api.Server
    ( Listen (..) )
import Cardano.Wallet.Jormungandr
    ( serveWallet )
import Cardano.Wallet.Jormungandr.Compatibility
    ( Jormungandr )
import Cardano.Wallet.Jormungandr.Launch
    ( withConfig )
import Cardano.Wallet.Jormungandr.Network
    ( JormungandrBackend (..) )
import Cardano.Wallet.Logging
    ( transformTextTrace )
import Cardano.Wallet.Primitive.AddressDerivation
    ( NetworkDiscriminant (..) )
import Cardano.Wallet.Primitive.Model
    ( BlockchainParameters (..) )
import Cardano.Wallet.Primitive.Types
    ( SyncTolerance (..) )
import Control.Concurrent.Async
    ( race )
import Control.Concurrent.MVar
    ( newEmptyMVar, putMVar, takeMVar )
import Control.Exception
    ( throwIO )
import Control.Tracer
    ( contramap )
import Data.Proxy
    ( Proxy (..) )
import Network.HTTP.Client
    ( defaultManagerSettings
    , managerResponseTimeout
    , newManager
    , responseTimeoutMicro
    )
import Network.Wai.Middleware.Logging
    ( ServerLog (..) )
import Test.Hspec
    ( Spec, SpecWith, after, describe, hspec, parallel )
import Test.Hspec.Extra
    ( aroundAll )
import Test.Integration.Framework.DSL
    ( Context (..), KnownCommand (..), tearDown )

import qualified Cardano.BM.Configuration.Model as CM
import qualified Cardano.Pool.MetricsSpec as MetricsSpec
import qualified Cardano.Wallet.Jormungandr.NetworkSpec as NetworkLayer
import qualified Data.Text as T
import qualified Test.Integration.Jormungandr.Scenario.API.StakePools as StakePoolsApiJormungandr
import qualified Test.Integration.Jormungandr.Scenario.API.Transactions as TransactionsApiJormungandr
import qualified Test.Integration.Jormungandr.Scenario.CLI.Launcher as LauncherCLI
import qualified Test.Integration.Jormungandr.Scenario.CLI.Mnemonics as MnemonicsJormungandr
import qualified Test.Integration.Jormungandr.Scenario.CLI.Server as ServerCLI
import qualified Test.Integration.Jormungandr.Scenario.CLI.StakePools as StakePoolsCliJormungandr
import qualified Test.Integration.Jormungandr.Scenario.CLI.Transactions as TransactionsCliJormungandr
import qualified Test.Integration.Scenario.API.Addresses as Addresses
import qualified Test.Integration.Scenario.API.ByronTransactions as ByronTransactions
import qualified Test.Integration.Scenario.API.ByronWallets as ByronWallets
import qualified Test.Integration.Scenario.API.Network as Network
import qualified Test.Integration.Scenario.API.Transactions as Transactions
import qualified Test.Integration.Scenario.API.Wallets as Wallets
import qualified Test.Integration.Scenario.CLI.Addresses as AddressesCLI
import qualified Test.Integration.Scenario.CLI.Miscellaneous as MiscellaneousCLI
import qualified Test.Integration.Scenario.CLI.Mnemonics as MnemonicsCLI
import qualified Test.Integration.Scenario.CLI.Network as NetworkCLI
import qualified Test.Integration.Scenario.CLI.Port as PortCLI
import qualified Test.Integration.Scenario.CLI.Transactions as TransactionsCLI
import qualified Test.Integration.Scenario.CLI.Wallets as WalletsCLI

-- | Define the actual executable name for the bridge CLI
instance KnownCommand Jormungandr where
    commandName = "cardano-wallet-jormungandr"

main :: forall t. (t ~ Jormungandr) => IO ()
main = withUtf8Encoding $ withLogging Nothing Info $ \(conf,tr) -> do
    let logging = (conf, transformTextTrace tr)
    hspec $ do
        describe "No backend required" $ do
            describe "Cardano.Wallet.NetworkSpec" $ parallel NetworkLayer.spec
            describe "Mnemonics CLI tests" $ parallel (MnemonicsCLI.spec @t)
            describe "Mnemonics CLI tests (Jormungandr)" $ parallel (MnemonicsJormungandr.spec @t)
            describe "Miscellaneous CLI tests" $ parallel (MiscellaneousCLI.spec @t)
            describe "Launcher CLI tests" $ parallel (LauncherCLI.spec @t)
            describe "Stake Pool Metrics" MetricsSpec.spec

        describe "API Specifications" $ specWithServer logging $ do
            Addresses.spec
            StakePoolsApiJormungandr.spec
            Transactions.spec
            TransactionsApiJormungandr.spec @t
            TransactionsCliJormungandr.spec @t
            Wallets.spec
            ByronWallets.spec
            ByronTransactions.spec
            Network.spec

        describe "CLI Specifications" $ specWithServer logging $ do
            AddressesCLI.spec @t
            ServerCLI.spec @t
            StakePoolsCliJormungandr.spec @t
            TransactionsCLI.spec @t
            WalletsCLI.spec @t
            PortCLI.spec @t
            NetworkCLI.spec @t

specWithServer
    :: (CM.Configuration, Trace IO ServerLog)
    -> SpecWith (Context Jormungandr)
    -> Spec
specWithServer logCfg@(_, trace) = aroundAll withContext . after tearDown
  where
    withContext :: (Context Jormungandr -> IO ()) -> IO ()
    withContext action = do
        ctx <- newEmptyMVar
        let setupContext wAddr nPort bp = do
                let baseUrl = "http://" <> T.pack (show wAddr) <> "/"
                let trText = contramap (fmap LogText) trace
                logInfo trText baseUrl
                let sixtySeconds = 60*1000*1000 -- 60s in microseconds
                manager <- (baseUrl,) <$> newManager (defaultManagerSettings
                    { managerResponseTimeout =
                        responseTimeoutMicro sixtySeconds
                    })
                faucet <- initFaucet
                putMVar ctx $ Context
                    { _cleanup = pure ()
                    , _manager = manager
                    , _nodePort = nPort
                    , _walletPort = sockAddrPort wAddr
                    , _faucet = faucet
                    , _feeEstimator = mkFeeEstimator (getFeePolicy bp)
                    , _target = Proxy
                    }
        race (takeMVar ctx >>= action) (withServer setupContext) >>=
            either pure (throwIO . ProcessHasExited "integration")

    withServer setup = withConfig $ \jmCfg ->
        serveWallet @'Testnet logCfg (SyncTolerance 10) Nothing "127.0.0.1"
            ListenOnRandomPort (Launch jmCfg) setup
