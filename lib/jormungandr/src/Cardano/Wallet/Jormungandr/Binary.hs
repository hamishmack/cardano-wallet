{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2018-2019 IOHK
-- License: MIT
--
-- The format is for the Shelley era as implemented by the Jörmungandr node.
--
-- It is described [here](https://github.com/input-output-hk/rust-cardano/blob/master/chain-impl-mockchain/doc/format.md)

module Cardano.Wallet.Jormungandr.Binary
    ( Block (..)
    , BlockHeader (..)
    , ConfigParam (..)
    , ConsensusVersion (..)
    , LeaderId (..)
    , LinearFee (..)
    , Message (..)
    , Milli (..)
    , getBlock
    , getBlockHeader
    , getMessage
    , getTransaction
    , putSignedTx
    , putTx

    -- * Coercion with business-domain
    , coerceBlock

    -- * Addresses
    , putAddress
    , getAddress
    , singleAddressFromKey

      -- * Legacy Decoders
    , decodeLegacyAddress

      -- * Re-export
    , Get
    , runGet
    , Put
    , runPut
    ) where

import Prelude

import Cardano.Crypto.Wallet
    ( XPub (xpubPublicKey) )
import Cardano.Wallet.Jormungandr.Environment
    ( KnownNetwork, Network (..), single )
import Cardano.Wallet.Jormungandr.Primitive.Types
    ( Tx (..) )
import Cardano.Wallet.Primitive.Types
    ( Address (..)
    , Coin (..)
    , Hash (..)
    , SlotId (..)
    , TxIn (..)
    , TxOut (..)
    , TxWitness (..)
    )
import Control.Applicative
    ( many )
import Control.Monad
    ( replicateM )
import Data.Binary.Get
    ( Get
    , bytesRead
    , getByteString
    , getWord16be
    , getWord32be
    , getWord64be
    , getWord8
    , isolate
    , label
    , lookAhead
    , runGet
    , skip
    )
import Data.Binary.Put
    ( Put
    , putByteString
    , putLazyByteString
    , putWord16be
    , putWord64be
    , putWord8
    , runPut
    )
import Data.Bits
    ( shift, (.&.) )
import Data.ByteString
    ( ByteString )
import Data.Proxy
    ( Proxy (..) )
import Data.Quantity
    ( Quantity (..) )
import Data.Word
    ( Word16, Word32, Word64, Word8 )

import qualified Cardano.Wallet.Primitive.Types as W
import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Read as CBOR
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

-- Do-notation is favoured over applicative syntax for readability:
{-# ANN module ("HLint: ignore Use <$>" :: String) #-}

data BlockHeader = BlockHeader
    { version :: Word16
    , contentSize :: Word32
    , slot :: SlotId
    , chainLength :: Word32
    , contentHash :: Hash "content"
    , parentHeaderHash :: Hash "BlockHeader"
    } deriving (Show, Eq)

data Block = Block BlockHeader [Message]
    deriving (Eq, Show)

getBlockHeader :: Get BlockHeader
getBlockHeader = label "getBlockHeader" $
    (fromIntegral <$> getWord16be) >>= \size -> isolate size $ do
        -- Common structure.
        version <- getWord16be
        contentSize <- getWord32be
        slotEpoch <- fromIntegral <$> getWord32be
        slotId <- fromIntegral <$> getWord32be
        chainLength <- getWord32be
        contentHash <- Hash <$> getByteString 32 -- or 256 bits
        parentHeaderHash <- Hash <$> getByteString 32
        -- Proof.
        -- There are three different types of proofs:
        -- 1. no proof (used for the genesis blockheader)
        -- 2. BFT
        -- 3. Praos / Genesis
        --
        -- We could make sure we get the right kind of proof, but we don't need to.
        -- Just checking that the length is not totally wrong, is much simpler
        -- and gives us sanity about the binary format being correct.
        read' <- fromIntegral <$> bytesRead
        let remaining = size - read'
        case remaining of
            0 -> skip remaining -- no proof
            96 -> skip remaining -- BFT
            616 -> skip remaining -- Praos/Genesis
            _ -> fail $ "BlockHeader proof has unexpected size " <> (show remaining)
        return $ BlockHeader
            { version
            , contentSize
            , slot = SlotId { epochNumber = slotEpoch, slotNumber = slotId }
            , chainLength
            , contentHash
            , parentHeaderHash
            }

getBlock :: Get Block
getBlock = label "getBlock" $ do
    header <- getBlockHeader
    msgs <- isolate (fromIntegral $ contentSize header) (many getMessage)
    return $ Block header msgs

{-------------------------------------------------------------------------------
                           Messages
-------------------------------------------------------------------------------}

-- | The block-body consists of messages. There are several types of messages.
--
--  Following, as closely as possible:
-- https://github.com/input-output-hk/rust-cardano/blob/e0616f13bebd6b908320bddb1c1502dea0d3305a/chain-impl-mockchain/src/message/mod.rs#L22-L29
data Message
    = Initial [ConfigParam]
    -- ^ Found in the genesis block.
    | Transaction (Tx, [TxWitness])
    -- ^ A standard signed transaction
    | UnimplementedMessage Int
    -- Messages not yet supported go there.
    deriving (Eq, Show)

-- | Decode a message (header + contents).
getMessage :: Get Message
getMessage = label "getMessage" $ do
    size <- fromIntegral <$> getWord16be
    contentType <- fromIntegral <$> getWord8
    let remaining = size - 1
    let unimpl = skip remaining >> return (UnimplementedMessage contentType)
    isolate remaining $ case contentType of
        0 -> Initial <$> getInitial
        1 -> unimpl
        2 -> Transaction <$> getTransaction
        3 -> unimpl
        4 -> unimpl
        5 -> unimpl
        other -> fail $ "Unexpected content type tag " ++ show other

-- | Decode the contents of a @Initial@-message.
getInitial :: Get [ConfigParam]
getInitial = label "getInitial" $ do
    len <- fromIntegral <$> getWord16be
    replicateM len getConfigParam

-- | Decode the contents of a @Transaction@-message.
getTransaction :: Get (Tx, [TxWitness])
getTransaction = label "getTransaction" $ do
    (ins, outs) <- getTokenTransfer
    let witnessCount = length ins
    wits <- replicateM witnessCount getWitness
    return (Tx ins outs, wits)
  where
    getWitness = do
        tag <- getWord8
        case tag of
            0 -> -- Legacy UTxO
                isolate 128 $ TxWitness <$> getByteString 128
            1 -> -- UTxO
                isolate 64 $ TxWitness <$> getByteString 64

            2 -> -- Account
                isolate 64 $ getByteString 64
                    *> error "unimplemented: Account witness"
            3 -> -- Multisig
                isolate 68 $ getByteString 68
                    *> error "unimplemented: Multisig witness"
            other ->
                fail $ "Invalid witness type: " ++ show other

    getTokenTransfer :: Get ([(TxIn, Coin)], [TxOut])
    getTokenTransfer = label "getTokenTransfer" $ do
        inCount <- fromIntegral <$> getWord8
        outCount <- fromIntegral <$> getWord8
        ins <- replicateM inCount getInput
        outs <- replicateM outCount getOutput
        return (ins, outs)
      where
        getInput = isolate 41 $ do
            -- NOTE: special value 0xff indicates account spending
            index <- fromIntegral <$> getWord8
            coin <- Coin <$> getWord64be
            tx <- Hash <$> getByteString 32
            return (TxIn tx index, coin)

        getOutput = do
            addr <- getAddress
            value <- Coin <$> getWord64be
            return $ TxOut addr value

{-------------------------------------------------------------------------------
                            Common Structure
-------------------------------------------------------------------------------}

putSignedTx :: (Tx, [TxWitness]) -> Put
putSignedTx (tx@(Tx inputs outputs), witnesses) = withSizeHeader16be $ do
    putWord8 2
    putWord8 $ fromIntegral $ length inputs
    putWord8 $ fromIntegral $ length outputs
    putTx tx
    mapM_ putWitness witnesses
  where
    -- Assumes the `TxWitness` has been faithfully constructed
    putWitness :: TxWitness -> Put
    putWitness (TxWitness bytes) = do
        putWord8 1
        putByteString bytes

putTx :: Tx -> Put
putTx (Tx inputs outputs) = do
    mapM_ putInput inputs
    mapM_ putOutput outputs
  where
    putInput (TxIn inputId inputIx, coin) = do
        -- NOTE: special value 0xff indicates account spending
        -- only old utxo/address scheme supported for now
        putWord8 $ fromIntegral inputIx
        putWord64be $ getCoin coin
        putByteString $ getHash inputId

    putOutput (TxOut address coin) = do
        putAddress address
        putWord64be $ getCoin coin

{-------------------------------------------------------------------------------
                            Config Parameters
-------------------------------------------------------------------------------}

data ConfigParam
    = Block0Date Word64
    -- ^ The official start time of the blockchain, in seconds since the Unix
    -- epoch.
    | Discrimination Network
    -- ^ Address discrimination. Testnet / Mainnet.
    | Consensus ConsensusVersion
    -- ^ Consensus version. BFT / Genesis Praos.
    | SlotsPerEpoch (Quantity "slot/epoch" Word32)
    -- ^ Number of slots in an epoch.
    | SlotDuration (Quantity "second/slot" Word8)
    -- ^ Slot duration in seconds.
    | EpochStabilityDepth (Quantity "block" Word32)
    -- ^ The length of the suffix of the chain (in blocks) considered unstable.
    | ConsensusGenesisPraosParamF Milli
    -- ^ Determines maximum probability of a stakeholder being elected as leader
    -- in a slot.
    | MaxNumberOfTransactionsPerBlock Word32
    -- ^ Maximum number of transactions in a block.
    | BftSlotsRatio Milli
    -- ^ Fraction of blocks to be created by BFT leaders.
    | AddBftLeader LeaderId
    -- ^ Add a BFT Leader
    | RemoveBftLeader LeaderId
    -- ^ Remove a BFT Leader
    | AllowAccountCreation Bool
    -- ^ Enable/disable account creation.
    | ConfigLinearFee LinearFee
    -- ^ Coefficients for fee calculations.
    | ProposalExpiration (Quantity "epoch" Word32)
    -- ^ Number of epochs until an update proposal expires.
    | KesUpdateSpeed (Quantity "second/update" Word32)
    -- ^ Maximum number of seconds per update for KES keys known by the system
    -- after start time.
    deriving (Eq, Show)

getConfigParam :: Get ConfigParam
getConfigParam = label "getConfigParam" $ do
    -- The tag and the size/length of the config param is stored in a single
    -- @Word16@.
    --
    -- 6 least-significant bits: length
    -- 12 most-significant bits: tag
    taglen <- getWord16be
    let tag = taglen `shift` (-6)
    let len = fromIntegral $ taglen .&. (63) -- 0b111111
    isolate len $ case tag of
        1 -> Discrimination <$> getNetwork
        2 -> Block0Date <$> getWord64be
        3 -> Consensus <$> getConsensusVersion
        4 -> SlotsPerEpoch . Quantity <$> getWord32be
        5 -> SlotDuration . Quantity <$> getWord8
        6 -> EpochStabilityDepth . Quantity <$> getWord32be
        8 -> ConsensusGenesisPraosParamF <$> getMilli
        9 -> MaxNumberOfTransactionsPerBlock <$> getWord32be
        10 -> BftSlotsRatio <$> getMilli
        11 -> AddBftLeader <$> getLeaderId
        12 -> RemoveBftLeader <$> getLeaderId
        13 -> AllowAccountCreation <$> getBool
        14 -> ConfigLinearFee <$> getLinearFee
        15 -> ProposalExpiration . Quantity <$> getWord32be
        16 -> KesUpdateSpeed . Quantity <$> getWord32be
        other -> fail $ "Invalid config param with tag " ++ show other

-- | Used to represent (>= 0) rational numbers as (>= 0) integers, by just
-- multiplying by 1000. For instance: '3.141592' is represented as 'Milli 3142'.
newtype Milli = Milli Word64
    deriving (Eq, Show)

newtype LeaderId = LeaderId ByteString
    deriving (Eq, Show)

data LinearFee = LinearFee
    { constant :: Quantity "lovelace" Word64
    , perByte :: Quantity "lovelace/byte" Word64
    , perCert :: Quantity "lovelace/cert" Word64
    } deriving (Eq, Show)

data ConsensusVersion = BFT | GenesisPraos
    deriving (Eq, Show)

getConsensusVersion :: Get ConsensusVersion
getConsensusVersion = label "getConsensusVersion" $ getWord16be >>= \case
    1 -> return BFT
    2 -> return GenesisPraos
    other -> fail $ "Unknown consensus version: " ++ show other

getNetwork :: Get Network
getNetwork = label "getNetwork" $ getWord8 >>= \case
    1 -> return Mainnet
    2 -> return Testnet
    other -> fail $ "Invalid network/discrimination value: " ++ show other

getMilli :: Get Milli
getMilli = label "getMilli" $ Milli <$> getWord64be

getLeaderId :: Get LeaderId
getLeaderId = label "getLeaderId" $ LeaderId <$> getByteString 32

getLinearFee :: Get LinearFee
getLinearFee = label "getLinearFee" $ do
    const' <- Quantity <$> getWord64be
    perByte <- Quantity <$> getWord64be
    perCert <- Quantity <$> getWord64be
    return $ LinearFee const' perByte perCert

getBool :: Get Bool
getBool = getWord8 >>= \case
    1 ->
        return True
    0 ->
        return False
    other ->
        fail $ "Unexpected integer: " ++ show other ++ ". Expected a boolean."

{-------------------------------------------------------------------------------
                            Addresses
-------------------------------------------------------------------------------}

getAddress :: Get Address
getAddress = do
    -- We use 'lookAhead' to not consume the header, and let it
    -- be included in the underlying Address ByteString.
    headerByte <- label "address header" . lookAhead $ getWord8
    let kind = kindValue headerByte
    case kind of
        0x3 -> Address <$> getByteString 33 -- single address
        0x4 -> Address <$> getByteString 65 -- grouped address
        0x5 -> Address <$> getByteString 65 -- account address
        0x6 -> Address <$> getByteString 33 -- multisig address
        other -> fail $ "Invalid address type: " ++ show other
  where
    kindValue :: Word8 -> Word8
    kindValue = (.&. 0b01111111)

putAddress :: Address -> Put
putAddress addr@(Address bs)
    | l == 33 = putByteString bs -- bootstrap or account addr
    | l == 65 = putByteString bs -- delegation addr
    | otherwise = fail
        $ "Address has unexpected length "
        ++ (show l)
        ++ ": " ++ show addr
  where l = BS.length bs

singleAddressFromKey :: forall n. KnownNetwork n => Proxy n -> XPub -> Address
singleAddressFromKey _ xPub = Address $ BL.toStrict $ runPut $ do
    putWord8 (single @n)
    isolatePut 32 $ putByteString (xpubPublicKey xPub)

{-------------------------------------------------------------------------------
                              Helpers
-------------------------------------------------------------------------------}

-- | Make sure a 'Put' encodes into a specific length
isolatePut :: Int -> Put -> Put
isolatePut l x = do
    let bs = runPut x
    if BL.length bs == (fromIntegral l)
    then putLazyByteString bs
    else fail $ "length was "
        ++ show (BL.length bs)
        ++ ", but expected to be "
        ++ (show l)

withSizeHeader16be :: Put -> Put
withSizeHeader16be x = do
    let bs = BL.toStrict $ runPut x
    putWord16be (fromIntegral $ BS.length bs)
    putByteString bs

{-------------------------------------------------------------------------------
                                Conversions
-------------------------------------------------------------------------------}

coerceBlock  :: Block -> W.Block Tx
coerceBlock (Block h msgs) =
    W.Block coerceHeader coerceMessages
  where
    coerceHeader = W.BlockHeader (slot h) (parentHeaderHash h)
    coerceMessages = msgs >>= \case
        Initial _ -> []
        Transaction (tx, _wits) -> return tx
        UnimplementedMessage _ -> []

{-------------------------------------------------------------------------------
                              Legacy Decoders
-------------------------------------------------------------------------------}

-- | Attempt decoding a 'ByteString' into an 'Address'. This merely checks that
-- the underlying bytestring has a "valid" structure / format without doing much
-- more.
decodeLegacyAddress :: ByteString -> Maybe Address
decodeLegacyAddress bytes =
    case CBOR.deserialiseFromBytes addressPayloadDecoder (BL.fromStrict bytes) of
        Right _ -> Just (Address bytes)
        Left _ -> Nothing
  where
    addressPayloadDecoder :: CBOR.Decoder s ()
    addressPayloadDecoder = ()
        <$ CBOR.decodeListLenCanonicalOf 2 -- Declare 2-Tuple
        <* CBOR.decodeTag -- CBOR Tag
        <* CBOR.decodeBytes -- Payload
        <* CBOR.decodeWord32 -- CRC