module Data.SignTyped.UtilSpec where

import Data.SignTyped.Util
import qualified Data.ByteString as BS
import qualified Test.Hspec as H

infixr 8 ^#
(^#) :: Integer -> Int -> Integer
(^#) = (^)

spec :: H.Spec
spec = do
    H.describe "Data.SignTyped.Util" $ do
        H.it "keccak256" $ do
            H.shouldBe
                (keccak256 "")
                "\xc5\xd2\x46\x01\x86\xf7\x23\x3c\x92\x7e\x7d\xb2\xdc\xc7\x03\xc0\
                \\xe5\x00\xb6\x53\xca\x82\x27\x3b\x7b\xfa\xd8\x04\x5d\x85\xa4\x70"
            H.shouldBe
                (keccak256 "sample string")
                "\x4f\x79\x02\x64\xcd\x3a\x8e\xcf\x7a\x97\x19\xb8\x1d\xda\x7e\x34\
                \\x96\x71\xc1\xab\xf7\xf7\x0d\x07\xa9\x71\x5b\x04\x47\x02\x19\x3b"
            H.shouldBe
                (keccak256 "\0\1\2\3")
                "\xd9\x8f\x2e\x81\x34\x92\x2f\x73\x74\x87\x03\xc8\xe7\x08\x4d\x42\
                \\xf1\x3d\x2f\xa1\x43\x99\x36\xef\x5a\x3a\xbc\xf5\x64\x6f\xe8\x3f"
        H.it "integer256" $ do
            H.shouldBe
                (integer256 0)
                (BS.replicate 32 0)
            H.shouldBe
                (integer256 1)
                (BS.replicate 31 0 <> "\1")
            H.shouldBe
                (integer256 256)
                (BS.replicate 30 0 <> "\1\0")
            H.shouldBe
                (integer256 (2 ^# 255))
                ("\128" <> BS.replicate 31 0)
            H.shouldBe
                (integer256 (-1))
                (BS.replicate 32 255)
            H.shouldBe
                (integer256 (-256))
                (BS.replicate 31 255 <> "\0")
            H.shouldBe
                (integer256 (- 2 ^# 255))
                ("\128" <> BS.replicate 31 0)
        H.it "readNumber" $ do
            H.shouldBe
                (readNumber "0")
                (Just 0)
            H.shouldBe
                (readNumber "12345")
                (Just 12345)
            H.shouldBe
                (readNumber "0x8000000000000000000000000000000000000000000000000000000000000000")
                (Just (2 ^# 255))
            H.shouldBe
                (readNumber "-0x8000000000000000000000000000000000000000000000000000000000000000")
                (Just (- 2 ^# 255))
        H.it "readChecksumAddress" $ do
            H.shouldBe
                (readChecksumAddress "0xbBbBBBBbbBBBbbbBbbBbbbbBBbBbbbbBbBbbBBbB")
                (Just $ BS.replicate 20 0xbb)
            H.shouldBe
                (readChecksumAddress "0xCcCCccccCCCCcCCCCCCcCcCccCcCCCcCcccccccC")
                (Just $ BS.replicate 20 0xcc)
            H.shouldBe
                (readChecksumAddress "0xccCCccccCCCCcCCCCCCcCcCccCcCCCcCcccccccC")
                Nothing {- invalid mixed-case -}
        H.it "readSafeTextString" $ do
            H.shouldBe
                (readSafeTextString "")
                (Just "")
            H.shouldBe
                (readSafeTextString "Some text")
                (Just "Some text")
            H.shouldBe
                (readSafeTextString "0x")
                (Just "")
            H.shouldBe
                (readSafeTextString "0x6f746865722074657874")
                (Just "other text")
            H.shouldBe
                (readSafeTextString "0x3078")
                (Just "0x")
            H.shouldBe
                (readSafeTextString "0xf09fa494")
                (Just "\129300")
            H.shouldBe
                (readSafeTextString "0xff")
                Nothing {- invalid utf-8 -}
        H.it "readHexBytes" $ do
            H.shouldBe
                (readHexBytes "0x")
                (Just "")
            H.shouldBe
                (readHexBytes "0X00010210203045AbCdEfaBcDeF")
                (Just "\x00\x01\x02\x10\x20\x30\x45\xab\xcd\xef\xab\xcd\xef")
            H.shouldBe
                (readHexBytes "0x0")
                Nothing
            H.shouldBe
                (readHexBytes "0xzz")
                Nothing
            H.shouldBe
                (readHexBytes "")
                Nothing
        H.it "encodeChecksumAddress" $ do
            H.shouldBe
                (encodeChecksumAddress $ BS.replicate 20 0xbb)
                "0xbBbBBBBbbBBBbbbBbbBbbbbBBbBbbbbBbBbbBBbB"
            H.shouldBe
                (encodeChecksumAddress $ BS.replicate 20 0xcc)
                "0xCcCCccccCCCCcCCCCCCcCcCccCcCCCcCcccccccC"
        H.it "encodeSafeTextString" $ do
            H.shouldBe
                (encodeSafeTextString "")
                ""
            H.shouldBe
                (encodeSafeTextString "Some text")
                "Some text"
            H.shouldBe
                (encodeSafeTextString "0x is a prefix")
                "0x3078206973206120707265666978"
            H.shouldBe
                (encodeSafeTextString "embedded \0")
                "0x656d6265646465642000"
        H.it "encodeHexByteString" $ do
            H.shouldBe
                (encodeHexByteString "")
                "0x"
            H.shouldBe
                (encodeHexByteString "Some text")
                "0x536f6d652074657874"
            H.shouldBe
                (encodeHexByteString "\x00\x01\x02\x10\x20\x30\x45\xab\xcd\xef")
                "0x00010210203045abcdef"
        H.it "encodeHexInteger" $ do
            H.shouldBe
                (encodeHexInteger 0)
                "0"
            H.shouldBe
                (encodeHexInteger 1)
                "0x1"
            H.shouldBe
                (encodeHexInteger 0x234)
                "0x234"
            H.shouldBe
                (encodeHexInteger (2 ^# 255))
                "0x8000000000000000000000000000000000000000000000000000000000000000"
            H.shouldBe
                (encodeHexInteger (- 0x234))
                "-0x234"
            H.shouldBe
                (encodeHexInteger (- 2 ^# 255))
                "-0x8000000000000000000000000000000000000000000000000000000000000000"
