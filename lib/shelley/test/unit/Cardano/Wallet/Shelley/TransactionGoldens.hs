{-# LANGUAGE DataKinds #-}
module Cardano.Wallet.Shelley.TransactionGoldens where

import Prelude

--import Cardano.Address.Derivation
--    ( XPrv, xprvFromBytes )
import Cardano.Wallet.Primitive.AddressDerivation
    ( hex )
import Cardano.Wallet.Primitive.Types
import Cardano.Wallet.Unsafe
    ( unsafeFromHex )
import Data.Maybe
    ( fromMaybe )
import Data.Text
    ( Text )

import qualified Codec.Binary.Bech32 as Bech32
import qualified Data.Text.Encoding as T

data TestVector addr key = TestVector
    { txins :: [(Hash "Tx", key, Int)]
    , txouts :: [(addr, Coin)]
--    , binary :: ByteString
    } deriving (Eq, Show)

data AddressTestVector addr key = AddressTestVector
    { network :: Int
    , paymentKey :: key
    , addr :: addr
    } deriving (Eq, Show)

txids :: [Hash "Tx"]
txids = map (Hash . unsafeFromHex)
    [ "25ea567beb08f9b82127c8d535af85d00e52adaaf981d034eef499f950b1e800"
--    , "742c45b1a7cf72a1d5a6cad15af664a25735f0fd0e78d589ad9b616742814178"
--    , "a88d29efeda1cfc80f9433659f1df46b179bcd47044f9a58c54598c7af57e0e7"
--    , "688d82d952a2181a85f8535a82afe9b303be8de866d9133de25d4900f0a8bfce"
    ]

baseAddresses :: [AddressTestVector Text Text]
baseAddresses =
    [AddressTestVector 0
        "38df9ed5c20d47c33d8fbf304861839b7cf1296b931121a4001c3fc20c7bd95cc2771576a180948b25931476a04712404dec35ffbb0718005384b6bb1580450dd121b5cc591e1c3a3e428943c3e771d7ff377bff680767c1c04cfb93fc6188da"
        "addr1qpu5vlrf4xkxv2qpwngf6cjhtw542ayty80v8dyr49rf5ewvxwdrt70qlcpeeagscasafhffqsxy36t90ldv06wqrk2qwmnp2v"
    ]
  where
--    prv = fromMaybe (error "could't construct a xprv from provided hex")
--        . xprvFromBytes
--        . unsafeFromHex
--
--    --addr = either (error . show) id (decodeAddress @'Mainnet)


tests :: [TestVector Text Text]
tests = do
    txid <- txids
    AddressTestVector _n k add <- baseAddresses
    return $
        TestVector
            [(txid, k, 0)]
            [(toHex add, ada 1)]


ada :: Int -> Coin
ada x = Coin (1000000 * (fromIntegral x))

toHex :: Text -> Text
toHex x =
    let
        (_hrp, dp) = either (error . show) id $ Bech32.decodeLenient x
    in
    T.decodeUtf8
        . hex
        . fromMaybe (error "no bech32 datapart")
        $ Bech32.dataPartToBytes dp

dev :: IO ()
dev = print $ map (toHex . addr) baseAddresses

