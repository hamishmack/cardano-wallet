{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: MIT
--
-- Provides the wallet layer functions that are used by API layer and uses both
-- "Cardano.Wallet.DB" and "Cardano.Wallet.Network" to realize its role as being
-- intermediary between the three.

module Cardano.Wallet
    (
    -- * Interface
      WalletLayer (..)

    -- * Errors
    , ErrCreateUnsignedTx (..)
    , ErrNoSuchWallet (..)
    , ErrSignTx (..)
    , ErrSubmitTx (..)
    , ErrUpdatePassphrase (..)
    , ErrWalletAlreadyExists (..)
    , ErrWithRootKey (..)
    , ErrWrongPassphrase (..)
    , ErrMkStdTx (..)
    , ErrPostTx (..)
    , ErrNetworkUnreachable (..)
    , ErrCoinSelection (..)
    , ErrAdjustForFee (..)

    -- * Construction
    , newWalletLayer
    ) where

import Prelude hiding
    ( log )

import Cardano.BM.Trace
    ( Trace, logDebug, logError, logInfo )
import Cardano.Wallet.DB
    ( DBLayer
    , ErrNoSuchWallet (..)
    , ErrWalletAlreadyExists (..)
    , PrimaryKey (..)
    )
import Cardano.Wallet.Network
    ( ErrNetworkUnreachable (..), ErrPostTx (..), NetworkLayer (..) )
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (RootK)
    , ErrWrongPassphrase (..)
    , Key
    , Passphrase
    , XPrv
    , changePassphrase
    , checkPassphrase
    , encryptPassphrase
    )
import Cardano.Wallet.Primitive.AddressDiscovery
    ( CompareDiscovery (..)
    , GenChange (..)
    , IsOurs (..)
    , IsOwned (..)
    , KnownAddresses (..)
    )
import Cardano.Wallet.Primitive.CoinSelection
    ( CoinSelection (..)
    , CoinSelectionOptions
    , ErrCoinSelection (..)
    , shuffle
    )
import Cardano.Wallet.Primitive.Fee
    ( ErrAdjustForFee (..)
    , FeeOptions (..)
    , adjustForFee
    , cardanoPolicy
    , computeFee
    )
import Cardano.Wallet.Primitive.Model
    ( Wallet
    , applyBlocks
    , availableUTxO
    , currentTip
    , getState
    , initWallet
    , newPending
    , updateState
    )
import Cardano.Wallet.Primitive.Types
    ( Address (..)
    , AddressState (..)
    , Block (..)
    , BlockHeader (..)
    , Coin (..)
    , DefineTx (..)
    , Direction (..)
    , SlotId (..)
    , Tx (..)
    , TxMeta (..)
    , TxOut (..)
    , TxStatus (..)
    , TxWitness
    , WalletDelegation (..)
    , WalletId (..)
    , WalletMetadata (..)
    , WalletName (..)
    , WalletPassphraseInfo (..)
    , WalletState (..)
    , slotRatio
    )
import Cardano.Wallet.Transaction
    ( ErrMkStdTx (..), TransactionLayer (..) )
import Cardano.Wallet.Unsafe
    ( unsafeRunExceptT )
import Control.Arrow
    ( first )
import Control.Concurrent
    ( ThreadId, forkIO, killThread, threadDelay )
import Control.Concurrent.MVar
    ( MVar, modifyMVar_, newMVar )
import Control.DeepSeq
    ( NFData )
import Control.Monad
    ( forM, unless )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Monad.Trans.Class
    ( lift )
import Control.Monad.Trans.Except
    ( ExceptT (..), runExceptT, throwE, withExceptT )
import Control.Monad.Trans.Maybe
    ( MaybeT (..), maybeToExceptT )
import Control.Monad.Trans.State
    ( runState, state )
import Data.Coerce
    ( coerce )
import Data.Functor
    ( ($>) )
import Data.Generics.Internal.VL.Lens
    ( view, (^.) )
import Data.Generics.Labels
    ()
import Data.List.NonEmpty
    ( NonEmpty )
import Data.Map.Strict
    ( Map )
import Data.Maybe
    ( mapMaybe )
import Data.Quantity
    ( Quantity (..) )
import Data.Text
    ( Text )
import Data.Time.Clock
    ( getCurrentTime )
import Fmt
    ( blockListF, pretty, (+|), (+||), (|+), (||+) )

import qualified Cardano.Wallet.DB as DB
import qualified Cardano.Wallet.Primitive.CoinSelection.Random as CoinSelection
import qualified Cardano.Wallet.Primitive.Types as W
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

{-------------------------------------------------------------------------------
                                 Types
-------------------------------------------------------------------------------}

data WalletLayer s t = WalletLayer
    { createWallet
        :: (Show s, NFData s, IsOurs s, DefineTx t)
        => WalletId
        -> WalletName
        -> s
        -> ExceptT ErrWalletAlreadyExists IO WalletId
        -- ^ Initialise and store a new wallet, returning its Id.

    , readWallet
        :: WalletId
        -> ExceptT ErrNoSuchWallet IO (Wallet s t, WalletMetadata)
        -- ^ Retrieve the wallet state for the wallet with the given ID.

    , updateWallet
        :: WalletId
        -> (WalletMetadata -> WalletMetadata)
        -> ExceptT ErrNoSuchWallet IO ()
        -- ^ Update the wallet metadata with the given update function.

    , updateWalletPassphrase
        :: WalletId
        -> (Passphrase "encryption-old", Passphrase "encryption-new")
        -> ExceptT ErrUpdatePassphrase IO ()
        -- ^ Change the wallet passphrase to the given passphrase.

    , listWallets
        :: IO [WalletId]
        -- ^ Retrieve a list of known wallets IDs.

    , removeWallet
        :: WalletId
        -> ExceptT ErrNoSuchWallet IO ()
        -- ^ Remove an existing wallet. Note that, there's no particular work to
        -- be done regarding the restoration worker as it will simply terminate
        -- on the next tick when noticing that the corresponding wallet is gone.

    , restoreWallet
        :: (DefineTx t)
        => WalletId
        -> ExceptT ErrNoSuchWallet IO ()
        -- ^ Restore a wallet from its current tip up to a given target
        -- (typically, the network tip).
        --
        -- It returns immediately and fail if the wallet is already beyond the
        -- given tip. It starts a worker in background which will fetch and
        -- apply remaining blocks until failure or, the target slot is reached.

    , listAddresses
        :: (IsOurs s, CompareDiscovery s, KnownAddresses s, DefineTx t)
        => WalletId
        -> ExceptT ErrNoSuchWallet IO [(Address, AddressState)]
        -- ^ List all addresses of a wallet with their metadata. Addresses
        -- are ordered from the most recently discovered to the oldest known.

    , createUnsignedTx
        :: (DefineTx t)
        => WalletId
        -> CoinSelectionOptions
        -> NonEmpty TxOut
        -> ExceptT ErrCreateUnsignedTx IO CoinSelection
        -- ^ Prepare a transaction and automatically select inputs from the
        -- wallet to cover the requested outputs. Note that this only run the
        -- coin selection for the given outputs. In order to construct (and
        -- sign) an actual transaction, have a look at 'signTx'.

    , signTx
        :: (Show s, NFData s, IsOwned s, GenChange s)
        => WalletId
        -> Passphrase "encryption"
        -> CoinSelection
        -> ExceptT ErrSignTx IO (Tx t, TxMeta, [TxWitness])
        -- ^ Produce witnesses and construct a transaction from a given
        -- selection. Requires the encryption passphrase in order to decrypt
        -- the root private key. Note that this doesn't broadcast the
        -- transaction to the network. In order to do so, have a look at
        -- 'submitTx'.

    , submitTx
        :: (DefineTx t)
        => WalletId
        -> (Tx t, TxMeta, [TxWitness])
        -> ExceptT ErrSubmitTx IO ()
        -- ^ Broadcast a (signed) transaction to the network.

    , attachPrivateKey
        :: WalletId
        -> (Key 'RootK XPrv, Passphrase "encryption")
        -> ExceptT ErrNoSuchWallet IO ()
        -- ^ Attach a given private key to a wallet. The private key is
        -- necessary for some operations like signing transactions or,
        -- generating new accounts.
    }

-- | Errors occuring when creating an unsigned transaction
data ErrCreateUnsignedTx
    = ErrCreateUnsignedTxNoSuchWallet ErrNoSuchWallet
    | ErrCreateUnsignedTxCoinSelection ErrCoinSelection
    | ErrCreateUnsignedTxFee ErrAdjustForFee
    deriving (Show, Eq)

-- | Errors occuring when signing a transaction
data ErrSignTx
    = ErrSignTx ErrMkStdTx
    | ErrSignTxNoSuchWallet ErrNoSuchWallet
    | ErrSignTxWithRootKey ErrWithRootKey
    deriving (Show, Eq)

-- | Errors occuring when submitting a signed transaction to the network
data ErrSubmitTx
    = ErrSubmitTxNetwork ErrPostTx
    | ErrSubmitTxNoSuchWallet ErrNoSuchWallet
    deriving (Show, Eq)

-- | Errors occuring when trying to change a wallet's passphrase
data ErrUpdatePassphrase
    = ErrUpdatePassphraseNoSuchWallet ErrNoSuchWallet
    | ErrUpdatePassphraseWithRootKey ErrWithRootKey
    deriving (Show, Eq)

-- | Errors occuring when trying to perform an operation on a wallet which
-- requires a private key, but none is attached to the wallet
data ErrWithRootKey
    = ErrWithRootKeyNoRootKey WalletId
    | ErrWithRootKeyWrongPassphrase WalletId ErrWrongPassphrase
    deriving (Show, Eq)

{-------------------------------------------------------------------------------
                                Worker Registry
-------------------------------------------------------------------------------}

-- | A simple registry to keep track of worker threads created for wallet
-- restoration. This way, we can clean-up workers threads early and don't have
-- to wait for them to fail with a an error message about the wallet being gone.
newtype WorkerRegistry = WorkerRegistry (MVar (Map WalletId ThreadId))

newRegistry :: IO WorkerRegistry
newRegistry = WorkerRegistry <$> newMVar mempty

registerWorker :: WorkerRegistry -> (WalletId, ThreadId) -> IO ()
registerWorker (WorkerRegistry mvar) (wid, threadId) =
    modifyMVar_ mvar (pure . Map.insert wid threadId)

cancelWorker :: WorkerRegistry -> WalletId -> IO ()
cancelWorker (WorkerRegistry mvar) wid =
    modifyMVar_ mvar (Map.alterF alterF wid)
  where
    alterF = \case
        Nothing -> pure Nothing
        Just threadId -> do
            -- NOTE: It is safe to kill a thread that is already dead.
            killThread threadId
            return Nothing

{-------------------------------------------------------------------------------
                                 Construction
-------------------------------------------------------------------------------}

-- | Create a new instance of the wallet layer.
newWalletLayer
    :: forall s t. ()
    => Trace IO Text
    -> Block (Tx t)
        -- ^ Very first block
    -> DBLayer IO s t
    -> NetworkLayer t IO
    -> TransactionLayer t
    -> IO (WalletLayer s t)
newWalletLayer tracer block0 db nw tl = do
    registry <- newRegistry
    return WalletLayer
        { createWallet = _createWallet
        , readWallet = _readWallet
        , updateWallet = _updateWallet
        , updateWalletPassphrase = _updateWalletPassphrase
        , listWallets = _listWallets
        , removeWallet = _removeWallet registry
        , restoreWallet = _restoreWallet registry
        , listAddresses = _listAddresses
        , createUnsignedTx = _createUnsignedTx
        , signTx = _signTx
        , submitTx = _submitTx
        , attachPrivateKey = _attachPrivateKey
        }
  where
    {---------------------------------------------------------------------------
                                       Wallets
    ---------------------------------------------------------------------------}

    _createWallet
        :: (Show s, NFData s, IsOurs s, DefineTx t)
        => WalletId
        -> WalletName
        -> s
        -> ExceptT ErrWalletAlreadyExists IO WalletId
    _createWallet wid wname s = do
        let checkpoint = initWallet block0 s
        currentTime <- liftIO getCurrentTime
        let metadata = WalletMetadata
                { name = wname
                , creationTime = currentTime
                , passphraseInfo = Nothing
                , status = Restoring minBound
                , delegation = NotDelegating
                }
        DB.createWallet db (PrimaryKey wid) checkpoint metadata $> wid

    _readWallet
        :: WalletId
        -> ExceptT ErrNoSuchWallet IO (Wallet s t, WalletMetadata)
    _readWallet wid = (,)
        <$> _readWalletCheckpoint wid
        <*> _readWalletMeta wid

    _readWalletMeta
        :: WalletId
        -> ExceptT ErrNoSuchWallet IO WalletMetadata
    _readWalletMeta wid = maybeToExceptT (ErrNoSuchWallet wid) $
        MaybeT $ DB.readWalletMeta db (PrimaryKey wid)

    _readWalletCheckpoint
        :: WalletId
        -> ExceptT ErrNoSuchWallet IO (Wallet s t)
    _readWalletCheckpoint wid = maybeToExceptT (ErrNoSuchWallet wid) $ do
        MaybeT $ DB.readCheckpoint db (PrimaryKey wid)

    _updateWallet
        :: WalletId
        -> (WalletMetadata -> WalletMetadata)
        -> ExceptT ErrNoSuchWallet IO ()
    _updateWallet wid modify = DB.withLock db $ do
        meta <- _readWalletMeta wid
        DB.putWalletMeta db (PrimaryKey wid) (modify meta)

    _updateWalletPassphrase
        :: WalletId
        -> (Passphrase "encryption-old", Passphrase "encryption-new")
        -> ExceptT ErrUpdatePassphrase IO ()
    _updateWalletPassphrase wid (old, new) = do
        withRootKey wid (coerce old) ErrUpdatePassphraseWithRootKey $ \xprv ->
            withExceptT ErrUpdatePassphraseNoSuchWallet $ do
                let xprv' = changePassphrase old new xprv
                _attachPrivateKey wid (xprv', coerce new)

    _listWallets
        :: IO [WalletId]
    _listWallets =
        fmap (\(PrimaryKey wid) -> wid) <$> DB.listWallets db

    _removeWallet
        :: WorkerRegistry
        -> WalletId
        -> ExceptT ErrNoSuchWallet IO ()
    _removeWallet re wid = do
        DB.withLock db . DB.removeWallet db . PrimaryKey $ wid
        liftIO $ cancelWorker re wid

    _restoreWallet
        :: (DefineTx t)
        => WorkerRegistry
        -> WalletId
        -> ExceptT ErrNoSuchWallet IO ()
    _restoreWallet re wid = do
        (w, _) <- _readWallet wid
        worker <- liftIO $ forkIO $ do
            runExceptT (networkTip nw) >>= \case
                Left e -> do
                    logError tracer $ "restoreSleep: " +|| e ||+ ""
                    restoreSleep wid (currentTip w)
                Right tip -> do
                    restoreStep wid (currentTip w, tip)
        liftIO $ registerWorker re (wid, worker)

    -- | Infinite restoration loop. We drain the whole available chain and try
    -- to catch up with the node. In case of error, we log it and wait a bit
    -- before retrying.
    --
    -- The function only terminates if the wallet has disappeared from the DB.
    restoreStep
        :: (DefineTx t)
        => WalletId
        -> (BlockHeader, BlockHeader)
        -> IO ()
    restoreStep wid (slot, tip) = do
        runExceptT (nextBlocks nw slot) >>= \case
            Left e -> do
                logError tracer $ "restoreStep: " +|| e ||+ ""
                restoreSleep wid slot
            Right [] -> do
                restoreSleep wid slot
            Right blocks -> do
                let next = view #header . last $ blocks
                runExceptT (restoreBlocks wid blocks (tip ^. #slotId)) >>= \case
                    Left (ErrNoSuchWallet _) -> logError tracer $
                        "restoreStep: wallet " +| wid |+ " is gone!"
                    Right () -> do
                        restoreStep wid (next, tip)

    -- | Wait a short delay before querying for blocks again. We do take this
    -- opportunity to also refresh the chain tip as it has probably increased
    -- in order to refine our syncing status.
    restoreSleep
        :: (DefineTx t)
        => WalletId
        -> BlockHeader
        -> IO ()
    restoreSleep wid slot = do
        let tenSeconds = 10000000 in threadDelay tenSeconds
        runExceptT (networkTip nw) >>= \case
            Left e -> do
                logError tracer $ "restoreSleep: " +|| e ||+ ""
                restoreSleep wid slot
            Right tip ->
                restoreStep wid (slot, tip)

    -- | Apply the given blocks to the wallet and update the wallet state,
    -- transaction history and corresponding metadata.
    restoreBlocks
        :: (DefineTx t)
        => WalletId
        -> [Block (Tx t)]
        -> SlotId -- ^ Network tip
        -> ExceptT ErrNoSuchWallet IO ()
    restoreBlocks wid blocks tip = do
        let (inf, sup) =
                ( view #slotId . header . head $ blocks
                , view #slotId . header . last $ blocks
                )
        liftIO $ logInfo tracer $
            "Applying blocks ["+| inf |+" ... "+| sup |+"]"

        -- NOTE
        -- Not as good as a transaction, but, with the lock, nothing can make
        -- the wallet disappear within these calls, so either the wallet is
        -- there and they all succeed, or it's not and they all fail.
        DB.withLock db $ do
            (cp, meta) <- _readWallet wid
            -- NOTE
            -- We only process non-empty blocks, though we still keep the last
            -- block of the list, even if empty, so that we correctly update the
            -- current tip of the wallet state.
            let nonEmpty = not . null . transactions
            let (h,q) = first (filter nonEmpty) $
                    splitAt (length blocks - 1) blocks
            let (txs, cp') = applyBlocks @s @t (h ++ q) cp
            let progress = slotRatio sup tip
            let status' = if progress == maxBound
                    then Ready
                    else Restoring progress
            let meta' = meta { status = status' } :: WalletMetadata
            liftIO $ logInfo tracer $
                "Tx History: " +|| length txs ||+ ""
            unless (null txs) $ liftIO $ logDebug tracer $ pretty $
                blockListF (snd <$> Map.elems txs)
            DB.putCheckpoint db (PrimaryKey wid) cp'
            DB.putTxHistory db (PrimaryKey wid) txs
            DB.putWalletMeta db (PrimaryKey wid) meta'

    {---------------------------------------------------------------------------
                                     Addresses
    ---------------------------------------------------------------------------}

    -- NOTE
    -- This implementation is rather inneficient and not intented for frequent
    -- use, in particular for exchanges or "big-players".
    _listAddresses
        :: (IsOurs s, CompareDiscovery s, KnownAddresses s, DefineTx t)
        => WalletId
        -> ExceptT ErrNoSuchWallet IO [(Address, AddressState)]
    _listAddresses wid = do
        (s, txs) <- DB.withLock db $ (,)
            <$> (getState <$> _readWalletCheckpoint wid)
            <*> liftIO (DB.readTxHistory db (PrimaryKey wid))
        let maybeIsOurs (TxOut a _) = if fst (isOurs a s)
                then Just a
                else Nothing
        let usedAddrs =
                Set.fromList $ concatMap (mapMaybe maybeIsOurs . outputs') txs
              where outputs' (tx, _) = W.outputs @t tx
        let knownAddrs =
                L.sortBy (compareDiscovery s) (knownAddresses s)
        let withAddressState addr =
                (addr, if addr `Set.member` usedAddrs then Used else Unused)
        return $ withAddressState <$> knownAddrs

    {---------------------------------------------------------------------------
                                    Transactions
    ---------------------------------------------------------------------------}

    _createUnsignedTx
        :: DefineTx t
        => WalletId
        -> CoinSelectionOptions
        -> NonEmpty TxOut
        -> ExceptT ErrCreateUnsignedTx IO CoinSelection
    _createUnsignedTx wid opts recipients = do
        (w, _) <- withExceptT ErrCreateUnsignedTxNoSuchWallet (_readWallet wid)
        let utxo = availableUTxO @s @t w
        (sel, utxo') <- withExceptT ErrCreateUnsignedTxCoinSelection $
            CoinSelection.random opts recipients utxo
        withExceptT ErrCreateUnsignedTxFee $ do
            let feeOpts = FeeOptions
                    { estimate = computeFee cardanoPolicy . estimateSize tl
                    , dustThreshold = minBound
                    }
            adjustForFee feeOpts utxo' sel

    _signTx
        :: (Show s, NFData s, IsOwned s, GenChange s)
        => WalletId
        -> Passphrase "encryption"
        -> CoinSelection
        -> ExceptT ErrSignTx IO (Tx t, TxMeta, [TxWitness])
    _signTx wid pwd (CoinSelection ins outs chgs) = DB.withLock db $ do
        (w, _) <- withExceptT ErrSignTxNoSuchWallet $ _readWallet wid
        let (changeOuts, s') = flip runState (getState w) $ forM chgs $ \c -> do
                addr <- state genChange
                return $ TxOut addr c
        allShuffledOuts <- liftIO $ shuffle (outs ++ changeOuts)
        withRootKey wid pwd ErrSignTxWithRootKey $ \xprv -> do
            let keyFrom = isOwned (getState w) (xprv, pwd)
            case mkStdTx tl keyFrom ins allShuffledOuts of
                Right (tx, wit) -> do
                    -- Safe because we have a lock and we already fetched the
                    -- wallet within this context.
                    liftIO . unsafeRunExceptT $
                        DB.putCheckpoint db (PrimaryKey wid) (updateState s' w)
                    let amtChng = fromIntegral $
                            sum (getCoin <$> chgs)
                    let amtInps = fromIntegral $
                            sum (getCoin . coin . snd <$> ins)
                    let meta = TxMeta
                            { status = Pending
                            , direction = Outgoing
                            , slotId = (currentTip w) ^. #slotId
                            , amount = Quantity (amtInps - amtChng)
                            }
                    return (tx, meta, wit)
                Left e ->
                    throwE $ ErrSignTx e

    _submitTx
        :: WalletId
        -> (Tx t, TxMeta, [TxWitness])
        -> ExceptT ErrSubmitTx IO ()
    _submitTx wid (tx, meta, wit) = do
        withExceptT ErrSubmitTxNetwork $ postTx nw (tx, wit)
        DB.withLock db $ withExceptT ErrSubmitTxNoSuchWallet $ do
            (w, _) <- _readWallet wid
            DB.putCheckpoint db (PrimaryKey wid) (newPending (tx, meta) w)

    {---------------------------------------------------------------------------
                                     Keystore
    ---------------------------------------------------------------------------}

    _attachPrivateKey
        :: WalletId
        -> (Key 'RootK XPrv, Passphrase "encryption")
        -> ExceptT ErrNoSuchWallet IO ()
    _attachPrivateKey wid (xprv, pwd) = do
        hpwd <- liftIO $ encryptPassphrase pwd
        DB.putPrivateKey db (PrimaryKey wid) (xprv, hpwd)
        DB.withLock db $ do
            meta <- _readWalletMeta wid
            now <- liftIO getCurrentTime
            let modify x =
                    x { passphraseInfo = Just (WalletPassphraseInfo now) }
            DB.putWalletMeta db (PrimaryKey wid) (modify meta)

    -- | Execute an action which requires holding a root XPrv
    withRootKey
        :: forall e a. ()
        => WalletId
        -> Passphrase "encryption"
        -> (ErrWithRootKey -> e)
        -> (Key 'RootK XPrv -> ExceptT e IO a)
        -> ExceptT e IO a
    withRootKey wid pwd embed action = do
        xprv <- withExceptT embed $ do
            lift (DB.readPrivateKey db (PrimaryKey wid)) >>= \case
                Nothing ->
                    throwE $ ErrWithRootKeyNoRootKey wid
                Just (xprv, hpwd) -> do
                    withExceptT (ErrWithRootKeyWrongPassphrase wid) $ ExceptT $
                        return $ checkPassphrase pwd hpwd
                    return xprv
        action xprv