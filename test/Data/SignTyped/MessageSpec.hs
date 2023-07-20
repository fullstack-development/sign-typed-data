{-# LANGUAGE QuasiQuotes #-}

module Data.SignTyped.MessageSpec where

import Data.SignTyped.EIP712Domain
import Data.SignTyped.Message
import Data.SignTyped.Test.NormalizeJson
import Data.SignTyped.Test.Structs
import Data.SignTyped.Util
import qualified Crypto.Secp256k1.Recovery as C
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BS.Lazy
import qualified Test.Hspec as H

exampleMailMessage :: Message
exampleMailMessage =
    either error id $
    makeMessage
        (SomeEIP712Domain exampleDomain)
        exampleMail

exampleMailMessageEncoding :: BS.Lazy.ByteString
exampleMailMessageEncoding = [njson|
    {
        "types": {
            "EIP712Domain": [
                {"type":"string", "name":"name"},
                {"type":"string", "name":"version"},
                {"type":"uint256", "name":"chainId"},
                {"type":"address", "name":"verifyingContract"}
            ],
            "Mail": [
                {"type":"Person", "name":"from"},
                {"type":"Person", "name":"to"},
                {"type":"string", "name":"contents"}
            ],
            "Person": [
                {"type":"string", "name":"name"},
                {"type":"address", "name":"wallet"}
            ]
        },
        "primaryType": "Mail",
        "domain": |] <> exampleDomainEncoding <> [njson|,
        "message": |] <> exampleMailEncoding <> [njson|
    }
|]

exampleTreeMessage :: Message
exampleTreeMessage =
    either error id $
    makeMessage
        (SomeEIP712Domain exampleDomain)
        exampleTree

exampleTreeMessageEncoding :: BS.Lazy.ByteString
exampleTreeMessageEncoding = [njson|
    {
        "types": {
            "EIP712Domain": [
                {"type": "string", "name": "name"},
                {"type": "string", "name": "version"},
                {"type": "uint256", "name": "chainId"},
                {"type": "address", "name": "verifyingContract"}
            ],
            "Forest": [
                {"type": "Tree[]", "name": "trees"}
            ],
            "Node": [
                {"type": "bool", "name": "fbool"},
                {"type": "string", "name": "fstring"},
                {"type": "uint256[4]", "name": "fuint256x4"}
            ],
            "Tree": [
                {"type": "Node", "name": "head"},
                {"type": "Forest", "name": "children"}
            ]
        },
        "primaryType": "Tree",
        "domain": |] <> exampleDomainEncoding <> [njson|,
        "message": |] <> exampleTreeEncoding <> [njson|
    }
|]

exampleMailSignature :: BS.ByteString
exampleMailSignature =
    maybe undefined id $
    readHexBytes
        "0x4355c47d63924e8a72e509b65029052eb6c299d53a04e167c5775fd466751c9d\
          \07299936d304c153f6443dfa05f40ff007d72911b6f72307f996231605b91562\
          \27"

testTwoWayJSON ::
    Message ->
    BS.Lazy.ByteString ->
    IO ()
testTwoWayJSON message expectedEncoding = do
    H.shouldBe
        (Aeson.encode message)
        expectedEncoding
    jsonValue <- Aeson.throwDecode expectedEncoding
    H.shouldBe
        (Aeson.toJSON message)
        jsonValue
    H.shouldBe
        (Aeson.eitherDecode expectedEncoding)
        (Right message)

spec :: H.Spec
spec =
    H.describe "Data.SignTyped.Message" $ do
        H.it "JSON" $ do
            testTwoWayJSON
                exampleMailMessage
                exampleMailMessageEncoding
            testTwoWayJSON
                exampleTreeMessage
                exampleTreeMessageEncoding
        H.it "messageChainId" $ do
            H.shouldBe
                (messageChainId exampleMailMessage)
                (Just 1)
        H.it "hashMessage" $ do
            H.shouldBe
                (hashMessage exampleMailMessage)
                "be609aee343fb3c4b28e1df9e632fca64fcfaede20f02e86244efddf30957bd2"
        H.it "signMessage" $ do
            C.withContext $ \ctx ->
                H.shouldBe
                    (encodeHexByteString $
                        signMessage
                            ctx
                            "c85ef7d79691fe79573b1a7064c19c1a9819ebdbd1faaab1a8ec92344438aaf4"
                            exampleMailMessage
                    )
                    (encodeHexByteString exampleMailSignature)
        H.it "recoverMessageSigner" $ do
            C.withContext $ \ctx ->
                H.shouldBe
                    (fmap encodeChecksumAddress $
                        recoverMessageSigner
                            ctx
                            exampleMailMessage
                            exampleMailSignature
                    )
                    (Just "0xCD2a3d9F938E13CD947Ec05AbC7FE734Df8DD826")
