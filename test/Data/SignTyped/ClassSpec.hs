module Data.SignTyped.ClassSpec where

import Data.SignTyped.Class
import Data.SignTyped.Structure
import Data.SignTyped.Test.Structs hiding (treeEthType)
import Data.SignTyped.Type.HList
import Data.SignTyped.Type.SName
import Data.SignTyped.Type.Some
import Data.SignTyped.Util
import qualified Test.Hspec as H

runSignableValueEmbedStr ::
    TypeContext ctx ->
    SignableValueHandle a ->
    EthType rep ->
    a ->
    Either String String
runSignableValueEmbedStr tc handle ethType value =
    fmap
        (\ethValue -> showsEthValue tc ethType ethValue "")
        (runEmbed tc $ signableValueEmbed handle ethType value)



showsNode :: Node -> ShowS
showsNode (Node b t [v0, v1, v2, v3]) =
    showString "Node {fbool = " .
    showString (if b then "true" else "false") .
    showString ", fstring = " .
    shows t .
    showString ", fuint256x4 = [" .
    shows v0 .
    showString ", " .
    shows v1 .
    showString ", " .
    shows v2 .
    showString ", " .
    shows v3 .
    showString "]}"
showsNode _ =
    undefined

showsTree :: Tree -> ShowS
showsTree (Tree th tc) =
    showString "Tree {head = " .
    showsNode th .
    showString ", children = Forest {trees = [" .
    goShowsChildren tc .
    showString "]}}"
  where
    goShowsChildren [] =
        id
    goShowsChildren (c : cs) =
        goShowsChildren1 c cs
    goShowsChildren1 c [] =
        showsTree c
    goShowsChildren1 c (c2 : cs2) =
        showsTree c .
        showString ", " .
        goShowsChildren1 c2 cs2



nodeStructHandle :: SignableValueHandle Node
nodeStructHandle =
    structValueHandle
        (SName @"Node")
        [ SignableStructMemberHandle
            "fbool"
            fbool
            boolValueHandle
        , SignableStructMemberHandle
            "fstring"
            fstring
            stringValueHandle
        , SignableStructMemberHandle
            "fuint256x4"
            fuint256x4
            (arrayValueHandle
                (numberValueHandle AtomicIntegerUnsigned 256)
                (Just 4)
            )
        ]

treeStructHandle :: SignableValueHandle Tree
treeStructHandle =
    structValueHandle
        (SName @"Tree")
        [ SignableStructMemberHandle
            "head"
            thead
            nodeStructHandle
        , SignableStructMemberHandle
            "children"
            tchildren
            forestStructHandle
        ]

forestStructHandle :: SignableValueHandle [Tree]
forestStructHandle =
    structValueHandle
        (SName @"Forest")
        [ SignableStructMemberHandle
            "trees"
            id
            (arrayValueHandle treeStructHandle Nothing)
        ]



expectedNodeTypeContext :: Some TypeContext
expectedNodeTypeContext =
    singletonTypeContext
        (SName @"Node")
        (
            MemberDef
                (EthTypeAtomic AtomicEthTypeBool)
                "fbool"
          :/
            MemberDef
                (EthTypeAtomic AtomicEthTypeString)
                "fstring"
          :/
            MemberDef
                (EthTypeArray
                    (EthTypeAtomic $ AtomicEthTypeInteger AtomicIntegerUnsigned 256)
                    (Just 4)
                )
                "fuint256x4"
          :/
            HEnd
        )

expectedTreeTypeContext :: Some TypeContext
expectedTreeTypeContext =
    expectedNodeTypeContext
  <>
    singletonTypeContext
        (SName @"Tree")
        (
            MemberDef
                (EthTypeStruct $ SName @"Node")
                "head"
          :/
            MemberDef
                (EthTypeStruct $ SName @"Forest")
                "children"
          :/
            HEnd
        )
  <>
    singletonTypeContext
        (SName @"Forest")
        (
            MemberDef
                (EthTypeArray
                    (EthTypeStruct $ SName @"Tree")
                    Nothing
                )
                "trees"
          :/
            HEnd
        )



spec :: H.Spec
spec = do
    H.describe "Data.SignTyped.Class" $ do
        withSome emptyTypeContext $ (\emptyTC -> do
            H.describe "boolValueHandle" $ do
                H.it "signableValueEmbed" $ do
                    H.shouldBe
                        (runSignableValueEmbedStr
                            emptyTC
                            boolValueHandle
                            (EthTypeAtomic AtomicEthTypeBool)
                            False
                        )
                        (Right "false")
                    H.shouldBe
                        (runSignableValueEmbedStr
                            emptyTC
                            boolValueHandle
                            (EthTypeAtomic AtomicEthTypeBool)
                            True
                        )
                        (Right "true")
                    H.shouldBe
                        (runSignableValueEmbedStr
                            emptyTC
                            boolValueHandle
                            (EthTypeArray undefined undefined)
                            True
                        )
                        (Left "$: Invalid eth type, expected \"bool\"")
            H.describe "numberValueHandle" $ do
                H.it "signableValueEmbed" $ do
                    H.shouldBe
                        (runSignableValueEmbedStr
                            emptyTC
                            (numberValueHandle undefined undefined)
                            (EthTypeAtomic $ AtomicEthTypeInteger AtomicIntegerUnsigned 8)
                            10
                        )
                        (Right "10")
                    H.shouldBe
                        (runSignableValueEmbedStr
                            emptyTC
                            (numberValueHandle undefined undefined)
                            (EthTypeAtomic $ AtomicEthTypeInteger AtomicIntegerUnsigned 8)
                            1000
                        )
                        (Left "$: Value is out of range")
                    H.shouldBe
                        (runSignableValueEmbedStr
                            emptyTC
                            (numberValueHandle undefined undefined)
                            (EthTypeAtomic $ AtomicEthTypeInteger AtomicIntegerUnsigned 32)
                            1000
                        )
                        (Right "1000")
                    H.shouldBe
                        (runSignableValueEmbedStr
                            emptyTC
                            (numberValueHandle undefined undefined)
                            (EthTypeAtomic $ AtomicEthTypeInteger AtomicIntegerUnsigned 8)
                            (-1)
                        )
                        (Left "$: Value is out of range")
                    H.shouldBe
                        (runSignableValueEmbedStr
                            emptyTC
                            (numberValueHandle undefined undefined)
                            (EthTypeAtomic $ AtomicEthTypeInteger AtomicIntegerSigned 8)
                            (-1)
                        )
                        (Right "-1")
            H.describe "bytesValueHandle" $ do
                H.it "signableValueEmbed" $ do
                    H.shouldBe
                        (runSignableValueEmbedStr
                            emptyTC
                            (bytesValueHandle undefined)
                            (EthTypeAtomic $ AtomicEthTypeBytes AtomicBytesSubtypeAddress)
                            (maybe undefined id $ readHexBytes "0x00112233445566778899aabbccddeeff00112233")
                        )
                        (Right "\"0x00112233445566778899AABbCCdDeeFf00112233\"")
                    H.shouldBe
                        (runSignableValueEmbedStr
                            emptyTC
                            (bytesValueHandle undefined)
                            (EthTypeAtomic $ AtomicEthTypeBytes AtomicBytesSubtypeAddress)
                            (maybe undefined id $ readHexBytes "0x00112233445566778899aabbccddeeff001122")
                        )
                        (Left "$: Invalid address")
                    H.shouldBe
                        (runSignableValueEmbedStr
                            emptyTC
                            (bytesValueHandle undefined)
                            (EthTypeAtomic $ AtomicEthTypeBytes AtomicBytesSubtypeAddress)
                            (maybe undefined id $ readHexBytes "0x00112233445566778899aabbccddeeff0011223344")
                        )
                        (Left "$: Invalid address")
                    H.shouldBe
                        (runSignableValueEmbedStr
                            emptyTC
                            (bytesValueHandle undefined)
                            (EthTypeAtomic $ AtomicEthTypeBytes $ AtomicBytesSubtypeShortBytes 20)
                            (maybe undefined id $ readHexBytes "0x00112233445566778899aabbccddeeff00112233")
                        )
                        (Right "\"0x00112233445566778899aabbccddeeff00112233\"")
                    H.shouldBe
                        (runSignableValueEmbedStr
                            emptyTC
                            (bytesValueHandle undefined)
                            (EthTypeAtomic $ AtomicEthTypeBytes $ AtomicBytesSubtypeShortBytes 20)
                            (maybe undefined id $ readHexBytes "0x00112233445566778899aabbccddeeff001122")
                        )
                        (Left "$: Invalid byte count")
                    H.shouldBe
                        (runSignableValueEmbedStr
                            emptyTC
                            (bytesValueHandle undefined)
                            (EthTypeAtomic $ AtomicEthTypeBytes AtomicBytesSubtypeLongBytes)
                            (maybe undefined id $ readHexBytes "0x00112233445566778899aabbccddeeff00112233")
                        )
                        (Right "\"0x00112233445566778899aabbccddeeff00112233\"")
                    H.shouldBe
                        (runSignableValueEmbedStr
                            emptyTC
                            (bytesValueHandle undefined)
                            (EthTypeAtomic $ AtomicEthTypeBytes AtomicBytesSubtypeLongBytes)
                            ""
                        )
                        (Right "\"0x\"")
            H.describe "stringValueHandle" $ do
                H.it "signableValueEmbed" $ do
                    H.shouldBe
                        (runSignableValueEmbedStr
                            emptyTC
                            stringValueHandle
                            (EthTypeAtomic AtomicEthTypeString)
                            ""
                        )
                        (Right "\"\"")
                    H.shouldBe
                        (runSignableValueEmbedStr
                            emptyTC
                            stringValueHandle
                            (EthTypeAtomic AtomicEthTypeString)
                            "sample text"
                        )
                        (Right "\"sample text\"")
            H.describe "arrayValueHandle" $ do
                H.it "signableValueEmbed" $ do
                    H.shouldBe
                        (runSignableValueEmbedStr
                            emptyTC
                            (arrayValueHandle (numberValueHandle undefined undefined) undefined)
                            (EthTypeArray (EthTypeAtomic $ AtomicEthTypeInteger AtomicIntegerSigned 8) Nothing)
                            []
                        )
                        (Right "[]")
                    H.shouldBe
                        (runSignableValueEmbedStr
                            emptyTC
                            (arrayValueHandle (numberValueHandle undefined undefined) undefined)
                            (EthTypeArray (EthTypeAtomic $ AtomicEthTypeInteger AtomicIntegerSigned 8) Nothing)
                            [1, 2, 3]
                        )
                        (Right "[1, 2, 3]")
                    H.shouldBe
                        (runSignableValueEmbedStr
                            emptyTC
                            (arrayValueHandle (numberValueHandle undefined undefined) undefined)
                            (EthTypeArray (EthTypeAtomic $ AtomicEthTypeInteger AtomicIntegerSigned 8) (Just 4))
                            [1, 2, 3, 4]
                        )
                        (Right "[1, 2, 3, 4]")
                    H.shouldBe
                        (runSignableValueEmbedStr
                            emptyTC
                            (arrayValueHandle (numberValueHandle undefined undefined) undefined)
                            (EthTypeArray (EthTypeAtomic $ AtomicEthTypeInteger AtomicIntegerSigned 8) (Just 4))
                            [1, 2, 3, 4, 5, 6]
                        )
                        (Left "$: Invalid array size")
                    H.shouldBe
                        (runSignableValueEmbedStr
                            emptyTC
                            (arrayValueHandle (numberValueHandle undefined undefined) undefined)
                            (EthTypeArray (EthTypeAtomic $ AtomicEthTypeInteger AtomicIntegerSigned 8) Nothing)
                            [1, 2, 1000]
                        )
                        (Left "$[2]: Value is out of range")
            )
        H.describe "structValueHandle" $ do
            H.it "signableValueEthType" $ do
                H.shouldBe
                    (signableValueEthType $
                        structValueHandle
                            (SName @"TestStruct")
                            undefined
                    )
                    (Some $ EthTypeStruct $ SName @"TestStruct")
            H.it "signableValueTypeContext (simple)" $ do
                H.shouldBe
                    (signableValueTypeContext $
                        structValueHandle
                            (SName @"TestStruct")
                            []
                    )
                    (singletonTypeContext
                        (SName @"TestStruct")
                        HEnd
                    )
                H.shouldBe
                    (signableValueTypeContext nodeStructHandle)
                    expectedNodeTypeContext
            H.it "signableValueTypeContext (recursive)" $ do
                H.shouldBe
                    (signableValueTypeContext forestStructHandle)
                    expectedTreeTypeContext
            H.it "signableValueEmbed" $
                withSome (signableValueTypeContext treeStructHandle) $ \treeTC ->
                withSome (signableValueEthType treeStructHandle) $ \treeEthType -> do
                    H.shouldBe
                        (runSignableValueEmbedStr
                            treeTC
                            treeStructHandle
                            treeEthType
                            exampleTree
                        )
                        (Right $ showsTree exampleTree "")
        H.describe "genericSignableValueHandle" $ do
            H.it "signableValueEthType" $ do
                H.shouldBe
                    (signableValueEthType $ signableValueDefaultHandle @Node)
                    (Some $ EthTypeStruct $ SName @"Node")
                H.shouldBe
                    (signableValueEthType $ signableValueDefaultHandle @Tree)
                    (Some $ EthTypeStruct $ SName @"Tree")
                H.shouldBe
                    (signableValueEthType $ signableValueDefaultHandle @Forest)
                    (Some $ EthTypeStruct $ SName @"Forest")
            H.it "signableValueTypeContext (recursive)" $ do
                H.shouldBe
                    (signableValueTypeContext $ signableValueDefaultHandle @Tree)
                    expectedTreeTypeContext
            H.it "signableValueEmbed" $
                withSome (signableValueTypeContext $ signableValueDefaultHandle @Tree) $ \treeTC ->
                withSome (signableValueEthType $ signableValueDefaultHandle @Tree) $ \treeEthType -> do
                    H.shouldBe
                        (runSignableValueEmbedStr
                            treeTC
                            (signableValueDefaultHandle @Tree)
                            treeEthType
                            exampleTree
                        )
                        (Right $ showsTree exampleTree "")
