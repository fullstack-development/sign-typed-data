{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Data.SignTyped.StructureSpec where

import Data.SignTyped.Structure
import Data.SignTyped.Type.HList
import Data.SignTyped.Type.SName
import Data.SignTyped.Type.Some
import Data.Type.Equality
import qualified Data.ByteString as BS
import qualified Test.Hspec as H

data Record = Record
    { r_uint8 :: Integer
    , r_int8 :: Integer
    , r_address :: BS.ByteString
    , r_bytes4 :: BS.ByteString
    , r_int8_array4 :: [Integer]
    }

recordStructDef =
    MemberDef
        (EthTypeAtomic $ AtomicEthTypeInteger AtomicIntegerUnsigned 8)
        "r_uint8"
  :/
    MemberDef
        (EthTypeAtomic $ AtomicEthTypeInteger AtomicIntegerSigned 8)
        "r_int8"
  :/
    MemberDef
        (EthTypeAtomic $ AtomicEthTypeBytes AtomicBytesSubtypeAddress)
        "r_address"
  :/
    MemberDef
        (EthTypeAtomic $ AtomicEthTypeBytes $ AtomicBytesSubtypeShortBytes 4)
        "r_bytes4"
  :/
    MemberDef
        (EthTypeArray
            (EthTypeAtomic $ AtomicEthTypeInteger AtomicIntegerSigned 8)
            (Just 4)
        )
        "r_int8_array4"
  :/
    HEnd

someTypeContext = singletonTypeContext (SName @"Record") recordStructDef

encodeRecord :: TypeContext ctx -> Record -> EthValue ctx ('EthReprStruct "Record")
encodeRecord tc recValue =
    withStructDef tc (SName @"Record") $ \sdef ->
        case testEquality sdef recordStructDef of
            Nothing -> error "bad type context"
            Just Refl ->
                EthValueStruct $
                    EthValueAtomic (r_uint8 recValue)
                  :/
                    EthValueAtomic (r_int8 recValue)
                  :/
                    EthValueAtomic (r_address recValue)
                  :/
                    EthValueAtomic (r_bytes4 recValue)
                  :/
                    EthValueArray (map EthValueAtomic (r_int8_array4 recValue))
                  :/
                    HEnd

runValidator :: Record -> [String]
runValidator recValue =
    withSome someTypeContext $ \tc ->
    validateEthValue tc (EthTypeStruct $ SName @"Record") (encodeRecord tc recValue)

spec :: H.Spec
spec = do
    H.describe "Data.SignTyped.Structure" $ do
        H.it "ethTypeName" $ do
            H.shouldBe
                (ethTypeName $ EthTypeAtomic AtomicEthTypeBool)
                "bool"
            H.shouldBe
                (ethTypeName $ EthTypeAtomic $ AtomicEthTypeInteger AtomicIntegerUnsigned 8)
                "uint8"
            H.shouldBe
                (ethTypeName $ EthTypeAtomic $ AtomicEthTypeInteger AtomicIntegerUnsigned 128)
                "uint128"
            H.shouldBe
                (ethTypeName $ EthTypeAtomic $ AtomicEthTypeInteger AtomicIntegerSigned 128)
                "int128"
            H.shouldBe
                (ethTypeName $ EthTypeAtomic $ AtomicEthTypeBytes AtomicBytesSubtypeAddress)
                "address"
            H.shouldBe
                (ethTypeName $ EthTypeAtomic $ AtomicEthTypeBytes $ AtomicBytesSubtypeShortBytes 1)
                "bytes1"
            H.shouldBe
                (ethTypeName $ EthTypeAtomic $ AtomicEthTypeBytes $ AtomicBytesSubtypeShortBytes 32)
                "bytes32"
            H.shouldBe
                (ethTypeName $ EthTypeAtomic $ AtomicEthTypeBytes AtomicBytesSubtypeLongBytes)
                "bytes"
            H.shouldBe
                (ethTypeName $ EthTypeAtomic AtomicEthTypeString)
                "string"
            H.shouldBe
                (ethTypeName $ EthTypeArray (EthTypeAtomic $ AtomicEthTypeInteger AtomicIntegerUnsigned 128) Nothing)
                "uint128[]"
            H.shouldBe
                (ethTypeName $ EthTypeArray (EthTypeAtomic AtomicEthTypeString) Nothing)
                "string[]"
            H.shouldBe
                (ethTypeName $ EthTypeArray (EthTypeAtomic AtomicEthTypeString) (Just 2))
                "string[2]"
            H.shouldBe
                (ethTypeName $ EthTypeArray (EthTypeArray (EthTypeAtomic AtomicEthTypeString) (Just 2)) (Just 4))
                "string[2][4]"
            H.shouldBe
                (ethTypeName $ EthTypeStruct $ SName @"SomeStruct")
                "SomeStruct"
        H.it "validateEthValue" $ do
            H.shouldBe
                (runValidator $
                    Record
                        { r_uint8 = 0
                        , r_int8 = 0
                        , r_address = BS.replicate 20 0xbb
                        , r_bytes4 = "\1\2\3\4"
                        , r_int8_array4 = [1, 2, 3, 4]
                        }
                )
                []
            H.shouldBe
                (runValidator $
                    Record
                        { r_uint8 = -1
                        , r_int8 = 1000
                        , r_address = BS.replicate 30 0xbb
                        , r_bytes4 = "\1\2\3\4\5"
                        , r_int8_array4 = [-500, 300, 0]
                        }
                )
                [ "$.r_uint8: Integer value -1 is outside the range of \"uint8\""
                , "$.r_int8: Integer value 1000 is outside the range of \"int8\""
                , "$.r_address: Byte value \"0xbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb\" \
                  \is not a valid \"address\""
                , "$.r_bytes4: Byte value \"0x0102030405\" is not a valid \"bytes4\""
                , "$.r_int8_array4: Invalid array size for \"int8[4]\""
                , "$.r_int8_array4[0]: Integer value -500 is outside the range of \"int8\""
                , "$.r_int8_array4[1]: Integer value 300 is outside the range of \"int8\""
                ]
            H.shouldBe
                (runValidator $
                    Record
                        { r_uint8 = 0
                        , r_int8 = 0
                        , r_address = ""
                        , r_bytes4 = ""
                        , r_int8_array4 = [1, 2, 3, 127, 128, -128, -129]
                        }
                )
                [ "$.r_address: Byte value \"0x\" is not a valid \"address\""
                , "$.r_bytes4: Byte value \"0x\" is not a valid \"bytes4\""
                , "$.r_int8_array4: Invalid array size for \"int8[4]\""
                , "$.r_int8_array4[4]: Integer value 128 is outside the range of \"int8\""
                , "$.r_int8_array4[6]: Integer value -129 is outside the range of \"int8\""
                ]
