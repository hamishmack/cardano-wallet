{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2020 IOHK
-- License: Apache-2.0
--
-- Working with Shelley transactions.

module Cardano.Wallet.Shelley.Transaction
    ( newTransactionLayer
    , genesisBlockFromTxOuts
    ) where

import Prelude

import Cardano.Address.Derivation
    ( XPrv )
import Cardano.Binary
    ( serialize' )
import Cardano.Crypto.DSIGN
    ( DSIGNAlgorithm (..), SignedDSIGN (..) )
import Cardano.Crypto.DSIGN.Ed25519
import Cardano.Wallet.Primitive.AddressDerivation
    ( Depth (..), NetworkDiscriminant (..), Passphrase, WalletKey (..) )
import Cardano.Wallet.Primitive.Types
    ( Address (..)
    , Block (..)
    , BlockHeader (..)
    , BlockchainParameters (..)
    , Coin (..)
    , EpochLength (..)
    , Hash (..)
    , ProtocolMagic (..)
    , SealedTx
    , SlotId (..)
    , Tx (..)
    , TxIn
    , TxOut (..)
    )
import Cardano.Wallet.Shelley.Compatibility
    ( Shelley
    , TPraosStandardCrypto
    , toCardanoLovelace
    , toCardanoTxIn
    , toCardanoTxOut
    , toSealed
    , toSlotNo
    )
import Cardano.Wallet.Transaction
    ( ErrMkTx (..), ErrValidateSelection, TransactionLayer (..) )
import Crypto.Error
    ( throwCryptoError )
import Crypto.Hash
    ( hash )
import Crypto.Hash.Algorithms
    ( Blake2b_256 )
import Data.ByteString
    ( ByteString )
import Data.Coerce
    ( coerce )
import Data.Maybe
    ( fromMaybe )
import Data.Proxy
    ( Proxy )
import Data.Quantity
    ( Quantity (..) )
import Fmt
    ( Buildable (..) )
import GHC.Stack
    ( HasCallStack )

import qualified Cardano.Api as Cardano
import qualified Cardano.Crypto.Wallet as CC
import qualified Crypto.PubKey.Ed25519 as Ed25519
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Shelley.Spec.Ledger.BaseTypes as SL
import qualified Shelley.Spec.Ledger.Keys as SL
import qualified Shelley.Spec.Ledger.Tx as SL

newTransactionLayer
    :: forall (n :: NetworkDiscriminant) k t.
        ( t ~ IO Shelley
        , WalletKey k
        )
    => Proxy n
    -> ProtocolMagic
    -> TransactionLayer t k
newTransactionLayer _proxy _protocolMagic = TransactionLayer
    { mkStdTx = notImplemented "mkStdTx"
    , mkDelegationJoinTx = notImplemented "mkDelegationJoinTx"
    , mkDelegationQuitTx = notImplemented "mkDelegationQuitTx"
    , decodeSignedTx = notImplemented "decodeSignedTx"
    , estimateSize = notImplemented "estimateSize"
    , estimateMaxNumberOfInputs = notImplemented "estimateMaxNumberOfInputs"
    , validateSelection = notImplemented "validateSelection"
    , allowUnbalancedTx = notImplemented "allowUnbalancedTx"
    }
  where
    _mkStdTx
        :: (Address -> Maybe (k 'AddressK XPrv, Passphrase "encryption"))
        -> SlotId
        -> [(TxIn, TxOut)]
        -> [TxOut]
        -> Either ErrMkTx (Tx, SealedTx)
    _mkStdTx keyFrom slot ownedIns outs = do
        let ins' = map (toCardanoTxIn . fst) ownedIns
        let outs' = map toCardanoTxOut outs
        let fee = Coin $ sum (map (getCoin . coin . snd) ownedIns)
                - sum (map (getCoin . coin) outs)
        let certs = []

        -- TODO: 1. Get the current epoch length
        -- TODO: 2. The epoch length can change in the era change between byron and
        -- shelley? It would probably be better to retrive a @SlotNo@ directly
        -- without converting from @SlotId@.
        let epochLength = EpochLength 1215

        let Cardano.TxUnsignedShelley unsigned = Cardano.buildShelleyTransaction
                ins'
                outs'
                (toSlotNo epochLength slot)
                (toCardanoLovelace fee)
                certs
        let bytes = serialize' unsigned
        keyWits <- mapM (fmap (signBody bytes) . lookupPrivateKey . address . snd) ownedIns
        let stx = SL.Tx
                unsigned
                (Set.fromList keyWits)
                Map.empty    -- script witnesses
                SL.SNothing  -- metadata
        return $ toSealed stx
      where
        lookupPrivateKey
            :: Address
            -> Either ErrMkTx (k 'AddressK XPrv, Passphrase "encryption")
        lookupPrivateKey addr =
            maybe (Left $ ErrKeyNotFoundForAddress addr) Right (keyFrom addr)

        signBody :: ByteString -> (k 'AddressK XPrv, Passphrase "encryption") -> SL.WitVKey TPraosStandardCrypto
        signBody body k =
            let
                sig = SignedDSIGN $ fromMaybe (error "error converting signatures")
                        $ rawDeserialiseSigDSIGN
                        $ k `sign` body
                vKey = SL.VKey $ VerKeyEd25519DSIGN $  throwCryptoError $ Ed25519.publicKey $ BS.take 32 $ CC.unXPub $ CC.toXPub $ getRawKey $ fst k
            in
                SL.WitVKey vKey sig

        sign
            :: (k 'AddressK XPrv, Passphrase "encryption")
            -> ByteString
            -> ByteString
        sign (k, pass) = CC.unXSignature . CC.sign pass (getRawKey k)



-- | Construct a ("fake") genesis block from genesis transaction outputs.
--
-- The genesis data on haskell nodes is not a block at all, unlike the block0 on
-- jormungandr. This function is a method to deal with the discrepancy.
genesisBlockFromTxOuts :: BlockchainParameters -> [TxOut] -> Block
genesisBlockFromTxOuts bp outs = Block
    { delegations  = []
    , header = BlockHeader
        { slotId =
            SlotId 0 0
        , blockHeight =
            Quantity 0
        , headerHash =
            coerce $ getGenesisBlockHash bp
        , parentHeaderHash =
            Hash (BS.replicate 32 0)
        }
    , transactions = mkTx <$> outs
    }
  where
    mkTx out@(TxOut (Address bytes) _) =
        Tx (Hash $ blake2b256 bytes) [] [out]

blake2b256 :: ByteString -> ByteString
blake2b256 =
    BA.convert . hash @_ @Blake2b_256

--------------------------------------------------------------------------------
-- Extra validations on coin selection
--

-- | Transaction with 0 output amount is tried
data ErrInvalidTxOutAmount -- FIXME: = ErrInvalidTxOutAmount

instance Buildable ErrInvalidTxOutAmount where
    build _ = "Invalid coin selection: at least one output is null."

type instance ErrValidateSelection (IO Shelley) = ErrInvalidTxOutAmount

notImplemented :: HasCallStack => String -> a
notImplemented what = error ("Not implemented: " <> what)
