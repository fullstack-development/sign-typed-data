{-# LANGUAGE QuasiQuotes #-}

module Data.SignTyped.Test.Structs where

import Data.SignTyped.Class
import Data.SignTyped.EIP712Domain
import Data.SignTyped.Structure
import Data.SignTyped.Test.NormalizeJson
import Data.SignTyped.Type.SName
import Data.SignTyped.Type.Some
import Data.SignTyped.Util
import GHC.Generics (Generic)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BS.Lazy
import qualified Data.Text as Text

data Node = Node
    { fbool :: Bool
    , fstring :: Text.Text
    , fuint256x4 :: [Integer]
    }
    deriving (Generic)
    deriving (SignableValue "Node") via (
        GenericSignable
            '[  SigningFieldModifier "fuint256x4" 'Nothing ('Just (AsBaseType "uint256" (AsSized 4 Integer)))
            ]
            Node
    )
    deriving anyclass (SignableValueDefault "Node")

data Tree = Tree
    { thead :: Node
    , tchildren :: [Tree]
    }
    deriving (Generic)
    deriving (SignableValue "Tree") via (
        GenericSignable
            '[  SigningFieldModifier "thead" ('Just "head") 'Nothing
            ,   SigningFieldModifier "tchildren" ('Just "children") ('Just Forest)
            ]
            Tree
    )
    deriving anyclass (SignableValueDefault "Tree")

newtype Forest = Forest
    { trees :: [Tree]
    }
    deriving (Generic)
    deriving (SignableValue "Forest") via (GenericSignable '[] Forest)
    deriving anyclass (SignableValueDefault "Forest")



data Person = Person
    { pname :: Text.Text
    , pwallet :: BS.ByteString
    }
    deriving (Generic)
    deriving (SignableValue "Person") via (
        GenericSignable
            '[  SigningFieldModifier "pname" ('Just "name") 'Nothing
            ,   SigningFieldModifier "pwallet" ('Just "wallet") ('Just (AsBaseType "address" BS.ByteString))
            ]
            Person
    )
    deriving anyclass (SignableValueDefault "Person")

data Mail = Mail
    { mfrom :: Person
    , mto :: Person
    , mcontents :: Text.Text
    }
    deriving (Generic)
    deriving (SignableValue "Mail") via (
        GenericSignable
            '[  SigningFieldModifier "mfrom" ('Just "from") 'Nothing
            ,   SigningFieldModifier "mto" ('Just "to") 'Nothing
            ,   SigningFieldModifier "mcontents" ('Just "contents") 'Nothing
            ]
            Mail
    )
    deriving anyclass (SignableValueDefault "Mail")



mailEthType :: EthType ('EthReprStruct "Mail")
mailEthType = EthTypeStruct $ SName @"Mail"

treeEthType :: EthType ('EthReprStruct "Tree")
treeEthType = EthTypeStruct $ SName @"Tree"



unifiedTypeContext :: Some TypeContext
unifiedTypeContext =
    signableValueTypeContext (signableValueDefaultHandle @Node) <>
    signableValueTypeContext (signableValueDefaultHandle @Tree) <>
    signableValueTypeContext (signableValueDefaultHandle @Forest) <>
    signableValueTypeContext (signableValueDefaultHandle @Person) <>
    signableValueTypeContext (signableValueDefaultHandle @Mail) <>
    signableValueTypeContext (signableValueDefaultHandle @ExampleDomain)

unifiedTypeContextEncoding :: BS.Lazy.ByteString
unifiedTypeContextEncoding = [njson|
    {
        "EIP712Domain": [
            {"type": "string", "name": "name"},
            {"type": "string", "name": "version"},
            {"type": "uint256", "name": "chainId"},
            {"type": "address", "name": "verifyingContract"}
        ],
        "Forest": [
            {"type": "Tree[]", "name": "trees"}
        ],
        "Mail": [
            {"type": "Person", "name": "from"},
            {"type": "Person", "name": "to"},
            {"type": "string", "name": "contents"}
        ],
        "Node": [
            {"type": "bool", "name": "fbool"},
            {"type": "string", "name": "fstring"},
            {"type": "uint256[4]", "name": "fuint256x4"}
        ],
        "Person": [
            {"type": "string", "name": "name"},
            {"type": "address", "name": "wallet"}
        ],
        "Tree": [
            {"type": "Node", "name": "head"},
            {"type": "Forest", "name": "children"}
        ]
    }
|]



type ExampleDomain =
    EIP712Domain (
        'EIP712DomainStructure
            ('Just String)
            ('Just String)
            ('Just Integer)
            ('Just BS.ByteString)
            'Nothing
    )

exampleDomain :: ExampleDomain
exampleDomain = EIP712Domain
    { eip712DomainName =
        FJust "Ether Mail"
    , eip712DomainVersion =
        FJust "1"
    , eip712DomainChainId =
        FJust 1
    , eip712DomainVerifyingContract =
        FJust $ BS.replicate 20 0xcc
    , eip712DomainSalt =
        FNothing
    }

exampleDomainEncoding :: BS.Lazy.ByteString
exampleDomainEncoding = [njson|
    {
        "name": "Ether Mail",
        "version": "1",
        "chainId": 1,
        "verifyingContract": "0xCcCCccccCCCCcCCCCCCcCcCccCcCCCcCcccccccC"
    }
|]



exampleMail :: Mail
exampleMail = Mail
    { mfrom = Person
        { pname = "Cow"
        , pwallet = maybe undefined id $ readChecksumAddress "0xCD2a3d9F938E13CD947Ec05AbC7FE734Df8DD826"
        }
    , mto = Person
        { pname = "Bob"
        , pwallet = maybe undefined id $ readChecksumAddress "0xbBbBBBBbbBBBbbbBbbBbbbbBBbBbbbbBbBbbBBbB"
        }
    , mcontents = "Hello, Bob!"
    }

exampleMailEncoding :: BS.Lazy.ByteString
exampleMailEncoding = [njson|
    {
        "from": {
            "name": "Cow",
            "wallet": "0xCD2a3d9F938E13CD947Ec05AbC7FE734Df8DD826"
        },
        "to": {
            "name": "Bob",
            "wallet": "0xbBbBBBBbbBBBbbbBbbBbbbbBBbBbbbbBbBbbBBbB"
        },
        "contents": "Hello, Bob!"
    }
|]



exampleTree :: Tree
exampleTree =
    Tree
        (Node False "text" [0, 1, 2, 2^(102::Int) - 1])
        [ Tree (Node False "A" [4, 8, 20, 3]) []
        , Tree (Node False "B" [5, 9, 21, 4])
            [ Tree (Node True "B1" [6, 0, 22, 5]) []
            , Tree (Node False "B2" [7, 1, 23, 6]) []
            ]
        , Tree (Node True "C" [8, 2, 24, 7]) []
        ]

exampleTreeEncoding :: BS.Lazy.ByteString
exampleTreeEncoding = [njson|
    {
        "head": {
            "fbool": false,
            "fstring": "text",
            "fuint256x4": [0, 1, 2, "0x3fffffffffffffffffffffffff"]
        },
        "children": {
            "trees": [
                {
                    "head": {
                        "fbool": false,
                        "fstring": "A",
                        "fuint256x4": [4, 8, 20, 3]
                    },
                    "children": {
                        "trees": []
                    }
                },
                {
                    "head":{
                        "fbool": false,
                        "fstring": "B",
                        "fuint256x4": [5, 9, 21, 4]
                    },
                    "children": {
                        "trees": [
                            {
                                "head": {
                                    "fbool": true,
                                    "fstring": "B1",
                                    "fuint256x4": [6, 0, 22, 5]
                                },
                                "children": {
                                    "trees": []
                                }
                            },
                            {
                                "head": {
                                    "fbool": false,
                                    "fstring": "B2",
                                    "fuint256x4": [7, 1, 23, 6]
                                },
                                "children": {
                                    "trees": []
                                }
                            }
                        ]
                    }
                },
                {
                    "head": {
                        "fbool": true,
                        "fstring": "C",
                        "fuint256x4": [8, 2, 24, 7]
                    },
                    "children": {
                        "trees": []
                    }
                }
            ]
        }
    }
|]
