module Data.SignTyped.EIP712Domain
    ( EIP712DomainSymbol
    , EIP712Domain (..)
    , SomeEIP712Domain (..)
    , EIP712DomainStructure (..)
    , EIP712DomainStructureNameType
    , EIP712DomainStructureVersionType
    , EIP712DomainStructureChainIdType
    , EIP712DomainStructureVerifyingContractType
    , EIP712DomainStructureSaltType
    , KnownEIP712DomainStructure
    , eip712DomainEthType
    , FMaybe (..)
    )
where

import Data.Kind
import Data.SignTyped.Class
import Data.SignTyped.Structure
import Data.SignTyped.Type.SCMaybe
import Data.SignTyped.Type.SName

{-| To avoid typos, use this constant for the canonical name of the domain struct, instead of typing the literal
directly. -}
type EIP712DomainSymbol = "EIP712Domain"

{-| The domain structure, from which the domain separator is derived.

    The parameter configures the presence and Haskell representation of its fields.

    The equality constraint is there to guide the type inference. -}
data EIP712Domain (opt :: EIP712DomainStructure)
    = (opt ~
        'EIP712DomainStructure
            (EIP712DomainStructureNameType opt)
            (EIP712DomainStructureVersionType opt)
            (EIP712DomainStructureChainIdType opt)
            (EIP712DomainStructureVerifyingContractType opt)
            (EIP712DomainStructureSaltType opt)
    ) =>
    EIP712Domain
        { eip712DomainName :: FMaybe (EIP712DomainStructureNameType opt)
        , eip712DomainVersion :: FMaybe (EIP712DomainStructureVersionType opt)
        , eip712DomainChainId :: FMaybe (EIP712DomainStructureChainIdType opt)
        , eip712DomainVerifyingContract :: FMaybe (EIP712DomainStructureVerifyingContractType opt)
        , eip712DomainSalt :: FMaybe (EIP712DomainStructureSaltType opt)
        }

deriving instance (KnownEIP712DomainStructure opt) => Show (EIP712Domain opt)

instance (KnownEIP712DomainStructure opt) => SignableValue EIP712DomainSymbol (EIP712Domain opt) where
    signableValueHandle =
        structValueHandle (SName @EIP712DomainSymbol) $
            withCMaybe
                @(SignableValue "string")
                @(EIP712DomainStructureNameType opt)
                []
                [ SignableStructMemberHandle
                    "name"
                    (getFJust . eip712DomainName)
                    (signableValueHandle @"string")
                ]
          <>
            withCMaybe
                @(SignableValue "string")
                @(EIP712DomainStructureVersionType opt)
                []
                [ SignableStructMemberHandle
                    "version"
                    (getFJust . eip712DomainVersion)
                    (signableValueHandle @"string")
                ]
          <>
            withCMaybe
                @(SignableValue "uint256")
                @(EIP712DomainStructureChainIdType opt)
                []
                [ SignableStructMemberHandle
                    "chainId"
                    (getFJust . eip712DomainChainId)
                    (signableValueHandle @"uint256")
                ]
          <>
            withCMaybe
                @(SignableValue "address")
                @(EIP712DomainStructureVerifyingContractType opt)
                []
                [ SignableStructMemberHandle
                    "verifyingContract"
                    (getFJust . eip712DomainVerifyingContract)
                    (signableValueHandle @"address")
                ]
          <>
            withCMaybe
                @(SignableValue "bytes32")
                @(EIP712DomainStructureSaltType opt)
                []
                [ SignableStructMemberHandle
                    "salt"
                    (getFJust . eip712DomainSalt)
                    (signableValueHandle @"bytes32")
                ]

instance (KnownEIP712DomainStructure opt) => SignableValueDefault EIP712DomainSymbol (EIP712Domain opt)

{-| A wrapper that combines a `EIP712Domain` with its `KnownEIP712DomainStructure` instance. -}
data SomeEIP712Domain = forall opt. (KnownEIP712DomainStructure opt) => SomeEIP712Domain (EIP712Domain opt)



{-| In the EIP-712 spec, the fields of the @EIP712Domain@ struct are optional. If some fields are not necessary
in a specific case, they can be removed from the type definition entirely. Since we set up the `SignableValue`
machinery to have struct definitions depend only on Haskell types, not values, that means each possible configuration
of the domain fields must be a separate Haskell type. This structure is the type-level parameter that distinguishes
between these configurations.

    In addition, we allow the Haskell representation of each field to be any Haskell type that has a `SignableValue`
instance for the appropriate Solidity typename. -}
data EIP712DomainStructure = EIP712DomainStructure
    { eip712DomainStructureNameType :: Maybe Type
    , eip712DomainStructureVersionType :: Maybe Type
    , eip712DomainStructureChainIdType :: Maybe Type
    , eip712DomainStructureVerifyingContractType :: Maybe Type
    , eip712DomainStructureSaltType :: Maybe Type
    }

type family EIP712DomainStructureNameType opt where
    EIP712DomainStructureNameType ('EIP712DomainStructure b _ _ _ _) = b

type family EIP712DomainStructureVersionType opt where
    EIP712DomainStructureVersionType ('EIP712DomainStructure _ b _ _ _) = b

type family EIP712DomainStructureChainIdType opt where
    EIP712DomainStructureChainIdType ('EIP712DomainStructure _ _ b _ _) = b

type family EIP712DomainStructureVerifyingContractType opt where
    EIP712DomainStructureVerifyingContractType ('EIP712DomainStructure _ _ _ b _) = b

type family EIP712DomainStructureSaltType opt where
    EIP712DomainStructureSaltType ('EIP712DomainStructure _ _ _ _ b) = b



{-| This typeclass is a "reified type synonym" for its context. Unlike a normal type synonym, however,
this name can also be used partially applied, for example, as a parameter to `KnownCMaybe`. -}
class
    ( KnownCMaybe (SignableValue "string") (EIP712DomainStructureNameType opt)
    , KnownCMaybe (SignableValue "string") (EIP712DomainStructureVersionType opt)
    , KnownCMaybe (SignableValue "uint256") (EIP712DomainStructureChainIdType opt)
    , KnownCMaybe (SignableValue "address") (EIP712DomainStructureVerifyingContractType opt)
    , KnownCMaybe (SignableValue "bytes32") (EIP712DomainStructureSaltType opt)
    , KnownCMaybe Show (EIP712DomainStructureNameType opt)
    , KnownCMaybe Show (EIP712DomainStructureVersionType opt)
    , KnownCMaybe Show (EIP712DomainStructureChainIdType opt)
    , KnownCMaybe Show (EIP712DomainStructureVerifyingContractType opt)
    , KnownCMaybe Show (EIP712DomainStructureSaltType opt)
    ) =>
    KnownEIP712DomainStructure opt

instance
    ( KnownCMaybe (SignableValue "string") (EIP712DomainStructureNameType opt)
    , KnownCMaybe (SignableValue "string") (EIP712DomainStructureVersionType opt)
    , KnownCMaybe (SignableValue "uint256") (EIP712DomainStructureChainIdType opt)
    , KnownCMaybe (SignableValue "address") (EIP712DomainStructureVerifyingContractType opt)
    , KnownCMaybe (SignableValue "bytes32") (EIP712DomainStructureSaltType opt)
    , KnownCMaybe Show (EIP712DomainStructureNameType opt)
    , KnownCMaybe Show (EIP712DomainStructureVersionType opt)
    , KnownCMaybe Show (EIP712DomainStructureChainIdType opt)
    , KnownCMaybe Show (EIP712DomainStructureVerifyingContractType opt)
    , KnownCMaybe Show (EIP712DomainStructureSaltType opt)
    ) =>
    KnownEIP712DomainStructure opt



{-| The canonical `EthType` for @EIP712Domain@. -}
eip712DomainEthType :: EthType ('EthReprStruct EIP712DomainSymbol)
eip712DomainEthType = EthTypeStruct (SName @EIP712DomainSymbol)
