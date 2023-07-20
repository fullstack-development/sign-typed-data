{-| An assortment of utility functions that are used across other modules. -}
module Data.SignTyped.Util
    ( symbolText
    , keccak256
    , integer256
    , readNumber
    , readChecksumAddress
    , readSafeTextString
    , readHexBytes
    , encodeChecksumAddress
    , encodeSafeTextString
    , encodeHexByteString
    , encodeHexInteger
    )
where

import Control.Monad
import Data.Bits
import Data.Proxy
import Data.Word
import Foreign.Ptr
import Foreign.Storable
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import qualified Crypto.Hash as H
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS.Internal
import qualified Data.ByteString.Unsafe as BS.Unsafe
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified Text.Read

{-| Take `KnownSymbol` value as a `Text.Text`. -}
symbolText :: forall (s :: Symbol). KnownSymbol s => Text.Text
symbolText = Text.pack (symbolVal (Proxy @s))

{-| @keccak256@ hash of a bytestring, as a bytestring.

    This is the main hashing function used in Ethereum. -}
keccak256 :: BS.ByteString -> BS.ByteString
keccak256 = BA.convert . H.hashWith H.Keccak_256

{-| Serialize an integer into a 32-byte big-endian 2's complement bytestring.

    @
        integer256 0      = unhex "0x0000000000000000000000000000000000000000000000000000000000000000"
        integer256 1      = unhex "0x0000000000000000000000000000000000000000000000000000000000000001"
        integer256 100    = unhex "0x0000000000000000000000000000000000000000000000000000000000000064"
        integer256 256    = unhex "0x0000000000000000000000000000000000000000000000000000000000000100"
        integer256 (-1)   = unhex "0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
        integer256 (-256) = unhex "0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff00"
    @ -}
integer256 :: Integer -> BS.ByteString
integer256 i =
    BS.pack $ reverse $ take 32 $ ibytes i
  where
    ibytes x =
        fromIntegral (x .&. 255) : ibytes (x `shiftR` 8)

{-| Try to parse the string as an integer.

    For simplicity, this re-uses the `Read` instance of `Integer`, which is quite permissive. For example, all of these
strings are accepted as valid representations of the number (-10):

    @
        "-10"
        "-0xa"
        "-0o12"
        "   -   10   "
        "\\n\\r\\t\\v\\f   -10   "
        "(((-10)))"
    @

    This function is used to parse @intN@ and @uintN@ values from JSON, so that other programs where native JSON
integers are limited by a relatively small number of bits could still produce JSONs with large integers by encoding
them in hexadecimal and stringifying. -}
readNumber :: Text.Text -> Maybe Integer
readNumber = Text.Read.readMaybe . Text.unpack

{-| Try to parse the string as an address, while also verifying the casing of its letter digits to be the correct
ERC-55 checksum.

    This function is used to parse @address@ values. -}
readChecksumAddress :: Text.Text -> Maybe BS.ByteString
readChecksumAddress t = do
    guard $ Text.isPrefixOf "0x" t || Text.isPrefixOf "0X" t
    guard $ Text.length t == 42
    bs <- readHexBytes t
    guard $ Text.drop 2 (encodeChecksumAddress bs) == Text.drop 2 t
    Just bs

{-| Try to parse the string as a potentially hex-encoded string. More specifically, if the input string starts with
@0x@ or @0X@, it is parsed as a hex-encoded bytestring and then parsed from UTF-8. Otherwise, it is
simply interpreted as a literal string.

    @
        readSafeTextString "" = Just ""
        readSafeTextString "Some text" = Just "Some text"
        readSafeTextString "0x" = Just ""
        readSafeTextString "0x6f746865722074657874" = Just "other text"
        readSafeTextString "0x3078" = Just "0x"
        readSafeTextString "0xf09fa494" = Just "\129300"
        readSafeTextString "0xff" = Nothing -- due to being invalid utf-8
    @

    This function is used to parse @string@ values. -}
readSafeTextString :: Text.Text -> Maybe Text.Text
readSafeTextString t
    | Text.isPrefixOf "0x" t || Text.isPrefixOf "0X" t = do
        bs <- readHexBytes t
        case Text.Encoding.decodeUtf8' bs of
            Right t2 -> Just t2
            Left _ -> Nothing
    | otherwise =
        Just t

{-| Try to parse the string as a hex-encoded bytestring.

    This function is used to parse @bytesN@ and @bytes@ values. -}
readHexBytes :: Text.Text -> Maybe BS.ByteString
readHexBytes t
    | Text.isPrefixOf "0x" t || Text.isPrefixOf "0X" t
    , Text.length t `rem` 2 == 0
    , Text.all isHexDigit (Text.drop 2 t) = do
        let rsize = Text.length t `quot` 2 - 1
        Just $ BS.Internal.unsafeCreate rsize $ \tptr -> do
            let bsrc = Text.Encoding.encodeUtf8 (Text.drop 2 t)
            let valueAt n =
                    unhexw (BS.Unsafe.unsafeIndex bsrc (2 * n)) * 16 +
                    unhexw (BS.Unsafe.unsafeIndex bsrc (2 * n + 1))
            forM_ [0 .. rsize - 1] $ \i ->
                poke (tptr `plusPtr` i) (valueAt i)
    | otherwise =
        Nothing
  where
    unhexw w
        | 65 <= w && w <= 70 = w - 55
        | 97 <= w && w <= 102 = w - 87
        | otherwise = w - 48
    isHexDigit c =
        ('0' <= c && c <= '9') ||
        ('A' <= c && c <= 'F') ||
        ('a' <= c && c <= 'f')

{-| Encode an address in hexadecimal, while also applying the ERC-55 mixed-case checksumming.

    This function is used to serialize @address@ values. -}
encodeChecksumAddress :: BS.ByteString -> Text.Text
encodeChecksumAddress addrBytes = do
    let addrHex = hexByteString addrBytes
    let totalBytes = BS.length addrBytes
    let csumBytes = min totalBytes 32
    let csum = BS.take csumBytes $ keccak256 addrHex
    Text.Encoding.decodeUtf8 $
        BS.Internal.unsafeCreate (totalBytes * 2 + 2) $ \tptr -> do
            poke tptr (48 :: Word8)
            poke (tptr `plusPtr` 1) (120 :: Word8)
            unsafeWriteByteStringHex (tptr `plusPtr` 2) addrBytes
            forM_ [0 .. csumBytes - 1] $ \i -> do
                let cb = BS.Unsafe.unsafeIndex csum i
                when (cb .&. 0x80 /= 0) $ do
                    let ip = tptr `plusPtr` (i * 2 + 2) :: Ptr Word8
                    w <- peek ip
                    when (w >= 97) $ do
                        poke ip (w - 32)
                when (cb .&. 0x08 /= 0) $ do
                    let ip = tptr `plusPtr` (i * 2 + 3) :: Ptr Word8
                    w <- peek ip
                    when (w >= 97) $ do
                        poke ip (w - 32)

{-| Encode a text string in such a way that it can later be parsed unambiguously. If the input string starts with
@0x@ or @0X@, it will be hex-encoded, otherwise the bytes are UTF-8 decoded and simply output literally.

    @
        encodeSafeTextString "" = ""
        encodeSafeTextString "Some text" = "Some text"
        encodeSafeTextString "0x is a prefix" = "0x3078206973206120707265666978"
    @

    This function is used to serialize @string@ values. -}
encodeSafeTextString :: Text.Text -> Text.Text
encodeSafeTextString txt
    | not (Text.isPrefixOf "0x" txt || Text.isPrefixOf "0X" txt)
    , not (Text.elem '\0' txt) =
        txt
    | otherwise =
        encodeHexByteString (Text.Encoding.encodeUtf8 txt)

{-| Encode a bytestring in hexadecimal.

    This function is used to serialize @bytesN@ and @bytes@ values. -}
encodeHexByteString :: BS.ByteString -> Text.Text
encodeHexByteString bs = "0x" <> Text.Encoding.decodeUtf8 (hexByteString bs)

{-| Encode an integer in hexadecimal.

    This function is used to serialize huge @intN@ and @uintN@ values. This allows other programs where native
JSON integers are limited by a relatively small number of bits to still parse and accept large integers without
loss of precision. -}
encodeHexInteger :: Integer -> Text.Text
encodeHexInteger i0
    | i0 == 0 =
        "0"
    | i0 < 0 =
        "-" <> encodeHexInteger (-i0)
    | i0 > 2^(256::Int) - 1 =
        error "integer is too large"
    | otherwise =
        Text.Encoding.decodeUtf8 $
            BS.Internal.unsafeCreateUptoN 66 $ \tptr -> do
                poke tptr (48 :: Word8)
                poke (tptr `plusPtr` 1) (120 :: Word8)
                digitCount <- writeHexDigits 0 (tptr `plusPtr` 2) i0
                reverseDigits (tptr `plusPtr` 2) (tptr `plusPtr` (digitCount + 1))
                pure $ digitCount + 2
  where
    writeHexDigits :: Int -> Ptr Word8 -> Integer -> IO Int
    writeHexDigits dn ptr i
        | i == 0 =
            pure dn
        | otherwise = do
            poke ptr $ hexw (fromIntegral i .&. 0xf)
            writeHexDigits (dn + 1) (ptr `plusPtr` 1) (i `shiftR` 4)
    reverseDigits :: Ptr Word8 -> Ptr Word8 -> IO ()
    reverseDigits p1 p2
        | p1 >= p2 =
            pure ()
        | otherwise = do
            v1 <- peek p1
            v2 <- peek p2
            poke p1 v2
            poke p2 v1
            reverseDigits (p1 `plusPtr` 1) (p2 `plusPtr` (-1))

hexByteString :: BS.ByteString -> BS.ByteString
hexByteString bs =
    BS.Internal.unsafeCreate (BS.length bs * 2) $ \tptr -> do
        unsafeWriteByteStringHex tptr bs

unsafeWriteByteStringHex :: Ptr Word8 -> BS.ByteString -> IO ()
unsafeWriteByteStringHex ptr bs = do
    forM_ [0 .. BS.length bs - 1] $ \i -> do
        let w = BS.Unsafe.unsafeIndex bs i
        poke (ptr `plusPtr` (2 * i)) (hexw (w `shiftR` 4))
        poke (ptr `plusPtr` (2 * i + 1)) (hexw (w .&. 15))

hexw :: Word8 -> Word8
hexw n
    | n < 10 = n + 48
    | otherwise = n + 87
