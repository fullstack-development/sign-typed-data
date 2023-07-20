module Data.SignTyped.HashingSpec where

import Data.SignTyped.Class
import Data.SignTyped.Hashing
import Data.SignTyped.Structure
import Data.SignTyped.Test.Structs
import Data.SignTyped.Type.SName
import Data.SignTyped.Type.Some
import Data.SignTyped.Util
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified Test.Hspec as H

nodeSignaturePair :: (Text.Text, BS.ByteString)
nodeSignaturePair =
    ("Node", "Node(bool fbool,string fstring,uint256[4] fuint256x4)")

treeSignaturePair :: (Text.Text, BS.ByteString)
treeSignaturePair =
    ("Tree", "Tree(Node head,Forest children)")

forestSignaturePair :: (Text.Text, BS.ByteString)
forestSignaturePair =
    ("Forest", "Forest(Tree[] trees)")

personSignaturePair :: (Text.Text, BS.ByteString)
personSignaturePair =
    ("Person", "Person(string name,address wallet)")

mailSignaturePair :: (Text.Text, BS.ByteString)
mailSignaturePair =
    ("Mail", "Mail(Person from,Person to,string contents)")

unhex :: Text.Text -> BS.ByteString
unhex = maybe undefined id . readHexBytes

nodeHash :: Node -> BS.ByteString
nodeHash (Node b t vs) =
    keccak256 (
        keccak256 (snd nodeSignaturePair)
      <>
        (if b then BS.replicate 31 0 <> "\1" else BS.replicate 32 0)
      <>
        keccak256 (Text.Encoding.encodeUtf8 t)
      <>
        keccak256 (
            foldMap integer256 vs
        )
    )

treeHash :: Tree -> BS.ByteString
treeHash (Tree hn cs) =
    keccak256 (
        keccak256 (snd treeSignaturePair <> snd forestSignaturePair <> snd nodeSignaturePair)
      <>
        nodeHash hn
      <>
        keccak256 (
            keccak256 (snd forestSignaturePair <> snd nodeSignaturePair <> snd treeSignaturePair)
          <>
            keccak256 (
                foldMap treeHash cs
            )
        )
    )

spec :: H.Spec
spec =
    withSome unifiedTypeContext $ \tc -> do
        H.describe "Data.SignTyped.Hashing" $ do
            H.it "structLocalContext" $ do
                H.shouldBe
                    (structLocalContext tc (SName @"Node"))
                    (Map.fromList [nodeSignaturePair])
                H.shouldBe
                    (structLocalContext tc (SName @"Tree"))
                    (Map.fromList [nodeSignaturePair, treeSignaturePair, forestSignaturePair])
                H.shouldBe
                    (structLocalContext tc (SName @"Forest"))
                    (Map.fromList [nodeSignaturePair, treeSignaturePair, forestSignaturePair])
                H.shouldBe
                    (structLocalContext tc (SName @"Person"))
                    (Map.fromList [personSignaturePair])
                H.shouldBe
                    (structLocalContext tc (SName @"Mail"))
                    (Map.fromList [personSignaturePair, mailSignaturePair])
            H.it "structTypeFullSignature" $ do
                H.shouldBe
                    (structTypeFullSignature tc (SName @"Node"))
                    (snd nodeSignaturePair)
                H.shouldBe
                    (structTypeFullSignature tc (SName @"Tree"))
                    (snd treeSignaturePair <> snd forestSignaturePair <> snd nodeSignaturePair)
                H.shouldBe
                    (structTypeFullSignature tc (SName @"Forest"))
                    (snd forestSignaturePair <> snd nodeSignaturePair <> snd treeSignaturePair)
                H.shouldBe
                    (structTypeFullSignature tc (SName @"Person"))
                    (snd personSignaturePair)
                H.shouldBe
                    (structTypeFullSignature tc (SName @"Mail"))
                    (snd mailSignaturePair <> snd personSignaturePair)
            H.describe "hashEthValue" $ do
                H.it "at bools" $ do
                    H.shouldBe
                        (encodeHexByteString $ hashEthValue tc
                            (EthTypeAtomic AtomicEthTypeBool)
                            (EthValueAtomic False)
                        )
                        "0x0000000000000000000000000000000000000000000000000000000000000000"
                    H.shouldBe
                        (encodeHexByteString $ hashEthValue tc
                            (EthTypeAtomic AtomicEthTypeBool)
                            (EthValueAtomic True)
                        )
                        "0x0000000000000000000000000000000000000000000000000000000000000001"
                H.it "at integers" $ do
                    H.shouldBe
                        (encodeHexByteString $ hashEthValue tc
                            (EthTypeAtomic $ AtomicEthTypeInteger undefined undefined)
                            (EthValueAtomic 0)
                        )
                        "0x0000000000000000000000000000000000000000000000000000000000000000"
                    H.shouldBe
                        (encodeHexByteString $ hashEthValue tc
                            (EthTypeAtomic $ AtomicEthTypeInteger undefined undefined)
                            (EthValueAtomic (-1))
                        )
                        "0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
                    H.shouldBe
                        (encodeHexByteString $ hashEthValue tc
                            (EthTypeAtomic $ AtomicEthTypeInteger undefined undefined)
                            (EthValueAtomic (2 ^ (255 :: Int)))
                        )
                        "0x8000000000000000000000000000000000000000000000000000000000000000"
                    H.shouldBe
                        (encodeHexByteString $ hashEthValue tc
                            (EthTypeAtomic $ AtomicEthTypeInteger undefined undefined)
                            (EthValueAtomic (- 2 ^ (255 :: Int) + 1))
                        )
                        "0x8000000000000000000000000000000000000000000000000000000000000001"
                    H.shouldBe
                        (encodeHexByteString $ hashEthValue tc
                            (EthTypeAtomic $ AtomicEthTypeInteger undefined undefined)
                            (EthValueAtomic 0xfedcba9876543210ffeeddccbbaa9988776655443322110044338877ccbb00ff)
                        )
                        "0xfedcba9876543210ffeeddccbbaa9988776655443322110044338877ccbb00ff"
                H.it "at bytes" $ do
                    H.shouldBe
                        (encodeHexByteString $ hashEthValue tc
                            (EthTypeAtomic $ AtomicEthTypeBytes AtomicBytesSubtypeAddress)
                            (EthValueAtomic $ unhex "0xfedcba9876543210ffeeddccbbaa998877665544"
                            )
                        )
                        "0x000000000000000000000000fedcba9876543210ffeeddccbbaa998877665544"
                    H.shouldBe
                        (encodeHexByteString $ hashEthValue tc
                            (EthTypeAtomic $ AtomicEthTypeBytes $ AtomicBytesSubtypeShortBytes 4)
                            (EthValueAtomic "\x23\x56\xab\xfe")
                        )
                        "0x2356abfe00000000000000000000000000000000000000000000000000000000"
                    H.shouldBe
                        (encodeHexByteString $ hashEthValue tc
                            (EthTypeAtomic $ AtomicEthTypeBytes AtomicBytesSubtypeLongBytes)
                            (EthValueAtomic "\x23\x56\xab\xfe")
                        )
                        (encodeHexByteString $ keccak256 "\x23\x56\xab\xfe")
                H.it "at strings" $ do
                    H.shouldBe
                        (encodeHexByteString $ hashEthValue tc
                            (EthTypeAtomic AtomicEthTypeString)
                            (EthValueAtomic "\0 text \1092\12354\129300")
                        )
                        (encodeHexByteString $ keccak256 $
                            "\0 text " <> unhex "0xd184" <> unhex "0xe38182" <> unhex "0xf09fa494")
                H.it "at arrays" $ do
                    H.shouldBe
                        (encodeHexByteString $ hashEthValue tc
                            (EthTypeArray undefined undefined)
                            (EthValueArray [])
                        )
                        (encodeHexByteString $ keccak256 "")
                    H.shouldBe
                        (encodeHexByteString $ hashEthValue tc
                            (EthTypeArray (EthTypeAtomic $ AtomicEthTypeInteger undefined undefined) undefined)
                            (EthValueArray
                                [ EthValueAtomic 0
                                , EthValueAtomic 1
                                , EthValueAtomic (2 ^ (255 :: Int))
                                , EthValueAtomic 0xfedcba9876543210ffeeddccbbaa9988776655443322110044338877ccbb00ff
                                , EthValueAtomic (-1)
                                ]
                            )
                        )
                        (encodeHexByteString $ keccak256 $
                            unhex "0x0000000000000000000000000000000000000000000000000000000000000000" <>
                            unhex "0x0000000000000000000000000000000000000000000000000000000000000001" <>
                            unhex "0x8000000000000000000000000000000000000000000000000000000000000000" <>
                            unhex "0xfedcba9876543210ffeeddccbbaa9988776655443322110044338877ccbb00ff" <>
                            unhex "0xffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
                        )
                    H.shouldBe
                        (encodeHexByteString $ hashEthValue tc
                            (EthTypeArray (EthTypeAtomic $ AtomicEthTypeBytes AtomicBytesSubtypeLongBytes) undefined)
                            (EthValueArray
                                [ EthValueAtomic ""
                                , EthValueAtomic "\0\10\200"
                                , EthValueAtomic "text"
                                ]
                            )
                        )
                        (encodeHexByteString $ keccak256 $
                            keccak256 "" <>
                            keccak256 "\0\10\200" <>
                            keccak256 "text"
                        )
                H.it "at structs" $ do
                    H.shouldBe
                        (encodeHexByteString $ hashEthValue tc (EthTypeStruct $ SName @"Mail") $
                            either undefined id $
                                runEmbed tc $
                                    signableValueEmbed
                                        (signableValueDefaultHandle @Mail)
                                        (EthTypeStruct $ SName @"Mail")
                                        (Mail
                                            (Person "A" (BS.replicate 20 0xbb))
                                            (Person "B" (BS.replicate 20 0xcc))
                                            "str"
                                        )
                        )
                        (encodeHexByteString $ keccak256 $
                            keccak256 (snd mailSignaturePair <> snd personSignaturePair)
                          <>
                            keccak256 (
                                keccak256 (snd personSignaturePair)
                              <>
                                keccak256 "A"
                              <>
                                unhex "0x000000000000000000000000bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
                            )
                          <>
                            keccak256 (
                                keccak256 (snd personSignaturePair)
                              <>
                                keccak256 "B"
                              <>
                                unhex "0x000000000000000000000000cccccccccccccccccccccccccccccccccccccccc"
                            )
                          <>
                            keccak256 "str"
                        )
                    H.shouldBe
                        (encodeHexByteString $ hashEthValue tc (EthTypeStruct $ SName @"Tree") $
                            either undefined id $
                                runEmbed tc $
                                    signableValueEmbed
                                        (signableValueDefaultHandle @Tree)
                                        (EthTypeStruct $ SName @"Tree")
                                        exampleTree
                        )
                        (encodeHexByteString $ treeHash exampleTree)
