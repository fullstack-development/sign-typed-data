module Data.SignTyped.JsonSpec where

import Data.SignTyped.Class
import Data.SignTyped.Json
import Data.SignTyped.Structure
import Data.SignTyped.Test.Structs
import Data.SignTyped.Type.SName
import Data.SignTyped.Type.Some
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding as Aeson.Encoding
import qualified Data.Aeson.Types as Aeson.Types
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BS.Lazy
import qualified Data.Map as Map
import qualified Test.Hspec as H

unifiedUnmarkedTypeContext :: UnmarkedTypeContext
unifiedUnmarkedTypeContext =
    Map.fromList
        [ ( "EIP712Domain"
          , [ UnmarkedMemberDef "string" "name"
            , UnmarkedMemberDef "string" "version"
            , UnmarkedMemberDef "uint256" "chainId"
            , UnmarkedMemberDef "address" "verifyingContract"
            ]
          )
        , ( "Forest"
          , [ UnmarkedMemberDef "Tree[]" "trees"
            ]
          )
        , ( "Mail"
          , [ UnmarkedMemberDef "Person" "from"
            , UnmarkedMemberDef "Person" "to"
            , UnmarkedMemberDef "string" "contents"
            ]
          )
        , ( "Node"
          , [ UnmarkedMemberDef "bool" "fbool"
            , UnmarkedMemberDef "string" "fstring"
            , UnmarkedMemberDef "uint256[4]" "fuint256x4"
            ]
          )
        , ( "Person"
          , [ UnmarkedMemberDef "string" "name"
            , UnmarkedMemberDef "address" "wallet"
            ]
          )
        , ( "Tree"
          , [ UnmarkedMemberDef "Node" "head"
            , UnmarkedMemberDef "Forest" "children"
            ]
          )
        ]

testTwoWayJSON ::
    TypeContext ctx ->
    EthType repr ->
    EthValue ctx repr ->
    BS.Lazy.ByteString ->
    IO ()
testTwoWayJSON tc ethType ethValue expectedEncoding = do
    H.shouldBe
        (Aeson.Encoding.encodingToLazyByteString $ ethValueToEncoding tc ethType ethValue)
        expectedEncoding
    jsonValue <- Aeson.throwDecode expectedEncoding
    H.shouldBe
        (ethValueToJSON tc ethType ethValue)
        jsonValue
    H.shouldBe
        (SomeEthValue tc ethType <$> Aeson.Types.parseEither (ethValueParseJSON tc ethType) jsonValue)
        (Right $ SomeEthValue tc ethType ethValue)

spec :: H.Spec
spec =
    withSome unifiedTypeContext $ \tc -> do
        H.describe "Data.SignTyped.Json" $ do
            H.it "toUnmarkedTypeContext" $ do
                H.shouldBe
                    (toUnmarkedTypeContext tc)
                    unifiedUnmarkedTypeContext
            H.it "parseUnmarkedTypeContext" $ do
                H.shouldBe
                    (Aeson.Types.parseEither parseUnmarkedTypeContext unifiedUnmarkedTypeContext)
                    (Right $ Some tc)
                H.shouldBe
                    (Aeson.Types.parseEither parseUnmarkedTypeContext $
                        Map.fromList
                            [ ( "Tree"
                              , [ UnmarkedMemberDef "Node" "head"
                                , UnmarkedMemberDef "Forest" "children"
                                ]
                              )
                            ]
                    )
                    (Left "Error in $: Type names referenced but not defined: \"Forest\", \"Node\"")
                H.shouldBe
                    (Aeson.Types.parseEither parseUnmarkedTypeContext $
                        Map.fromList
                            [ ( "Tree"
                              , [ UnmarkedMemberDef "[[[" "xs"
                                ]
                              )
                            ]
                    )
                    (Left "Error in $.Tree[0]: Invalid type: \"[[[\"")
                H.shouldBe
                    (Aeson.Types.parseEither parseUnmarkedTypeContext $
                        Map.fromList
                            [ ( "Tree"
                              , [ UnmarkedMemberDef "uint256" "x"
                                , UnmarkedMemberDef "uint256" "x"
                                ]
                              )
                            ]
                    )
                    (Left "Error in $.Tree: Member name appears more than once: \"x\"")
            H.it "UnmarkedTypeContext JSON" $ do
                H.shouldBe
                    (Aeson.encode $ toUnmarkedTypeContext tc)
                    unifiedTypeContextEncoding
                H.shouldBe
                    (Aeson.eitherDecode unifiedTypeContextEncoding)
                    (Right unifiedUnmarkedTypeContext)
            H.it "value JSON" $ do
                testTwoWayJSON
                    tc
                    (EthTypeArray (EthTypeAtomic AtomicEthTypeBool) Nothing)
                    (EthValueArray $ map EthValueAtomic [False, True])
                    "[false,true]"
                testTwoWayJSON
                    tc
                    (EthTypeArray (EthTypeAtomic $ AtomicEthTypeInteger AtomicIntegerSigned 256) Nothing)
                    (EthValueArray $ map EthValueAtomic [0, 1, -1, 2^(64::Int), -2^(64::Int)])
                    "[0,1,-1,\"0x10000000000000000\",\"-0x10000000000000000\"]"
                testTwoWayJSON
                    tc
                    (EthTypeArray (EthTypeAtomic $ AtomicEthTypeBytes AtomicBytesSubtypeAddress) Nothing)
                    (EthValueArray $ map EthValueAtomic [BS.replicate 20 0xbb, BS.replicate 20 0xcc])
                    "[\"0xbBbBBBBbbBBBbbbBbbBbbbbBBbBbbbbBbBbbBBbB\"\
                    \,\"0xCcCCccccCCCCcCCCCCCcCcCccCcCCCcCcccccccC\"]"
                testTwoWayJSON
                    tc
                    (EthTypeAtomic $ AtomicEthTypeBytes $ AtomicBytesSubtypeShortBytes 4)
                    (EthValueAtomic "\x12\x56\xfe\xbc")
                    "\"0x1256febc\""
                testTwoWayJSON
                    tc
                    (EthTypeAtomic $ AtomicEthTypeBytes AtomicBytesSubtypeLongBytes)
                    (EthValueAtomic "\x12\x56\xfe\xbc")
                    "\"0x1256febc\""
                testTwoWayJSON
                    tc
                    (EthTypeAtomic AtomicEthTypeString)
                    (EthValueAtomic "text")
                    "\"text\""
                testTwoWayJSON
                    tc
                    (EthTypeAtomic AtomicEthTypeString)
                    (EthValueAtomic "0xtext")
                    "\"0x307874657874\""
                testTwoWayJSON
                    tc
                    (EthTypeStruct $ SName @"EIP712Domain")
                    (either undefined id $
                        runEmbed tc $
                            signableValueEmbed
                                signableValueDefaultHandle
                                (EthTypeStruct $ SName @"EIP712Domain")
                                exampleDomain
                    )
                    exampleDomainEncoding
                testTwoWayJSON
                    tc
                    mailEthType
                    (either undefined id $
                        runEmbed tc $
                            signableValueEmbed
                                signableValueDefaultHandle
                                mailEthType
                                exampleMail
                    )
                    exampleMailEncoding
                testTwoWayJSON
                    tc
                    treeEthType
                    (either undefined id $
                        runEmbed tc $
                            signableValueEmbed
                                signableValueDefaultHandle
                                treeEthType
                                exampleTree
                    )
                    exampleTreeEncoding
                (do
                    jsonValue <- Aeson.throwDecode "{}"
                    H.shouldBe
                        (SomeEthValue tc treeEthType <$>
                            Aeson.Types.parseEither
                                (ethValueParseJSON tc treeEthType)
                                jsonValue
                        )
                        (Right $ SomeEthValue tc treeEthType $ defaultEthValue tc treeEthType)
                 )
