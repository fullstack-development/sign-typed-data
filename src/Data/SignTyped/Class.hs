{-# OPTIONS_GHC -Wno-redundant-constraints -Wno-identities #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.SignTyped.Class
    {-* High-level entrypoints -}
    ( hashSignable
    , hashSignableWith

    {-* Typeclasses -}
    , SignableValue (..)
    , SignableValueDefault
    , signableValueDefaultHandle

    {-* Generic implementation -}
    , GenericSignable (..)
    , SigningModifier (..)
    , genericSignableValueHandle

    {-* Modifier wrappers -}
    , AsBaseType (..)
    , AsSized (..)

    {-* Simple value handles -}
    , boolValueHandle
    , numberValueHandle
    , bytesValueHandle
    , stringValueHandle
    , arrayValueHandle

    {-* Struct handle -}
    , structValueHandle
    , SignableStructMemberHandle (..)

    {-* SignableValueHandle internals -}
    , SignableValueHandle (..)
    , contramap
    , contramapSignableEither
    , TypeContextBuilder (..)
    , executeTypeContextBuilder

    {-* Value embedding monad -}
    , Embed (..)
    , runEmbed
    , withEmbedLocation
    , withEmbedLocationArrayElement
    , withEmbedLocationStructMember
    , askTypeContext
    , getStructDef

    {-* Generic implementation internals -}
    , GSignableRep (..)
    , ModifiedFieldName
    , ModifiedFieldType
    , ValidateModifiers
    , AssertValidation
    )
where

import Control.Applicative
import Control.Monad
import Data.Coerce
import Data.Foldable
import Data.Functor.Contravariant
import Data.Int
import Data.Kind
import Data.Maybe
import Data.SignTyped.Hashing
import Data.SignTyped.Structure
import Data.SignTyped.Type.HList
import Data.SignTyped.Type.HMap
import Data.SignTyped.Type.SName
import Data.SignTyped.Type.Some
import Data.SignTyped.Util
import Data.Type.Bool
import Data.Type.Equality
import Data.Word
import GHC.Generics
import GHC.TypeLits
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified Language.Haskell.TH as TH



{-| Perform EIP-712 @encodeData@ on the given value with its type's default handle. -}
hashSignable ::
    forall a baseType.
    (SignableValueDefault baseType a) =>
    a ->
    Either String BS.ByteString
hashSignable =
    hashSignableWith (signableValueHandle @baseType @a)

{-| Perform EIP-712 @encodeData@ on the given value using the given handle. -}
hashSignableWith ::
    forall a.
    SignableValueHandle a ->
    a ->
    Either String BS.ByteString
hashSignableWith valueHandle x =
    withSome (signableValueTypeContext valueHandle) $ \tc ->
    withSome (signableValueEthType valueHandle) $ \rootEthType ->
    runEmbed tc $ do
        rootEthValue <- signableValueEmbed valueHandle rootEthType x
        pure $ hashEthValue tc rootEthType rootEthValue



{-| @SignableValue baseType a@ allows a Haskell value of type @a@ to be represented as a Solidity value
with the typename @baseType@.

    The relation between Solidity and Haskell types is many-to-many. For example, Solidity's @uint64@
can be produced from any of `Word`, `Int`, `Data.Natural.Natural` or `Integer`. On the other hand,
Haskell's `Integer` can be mapped to any of Solidity's numeric types.

    The actual implementation is delegated to a `SignableValueHandle` object, which allows us some additional
flexibility:

    * The `SignableValue` itself is treated as a /default/ implementation rather than the /canonical/ one.

    * To define an instance of `SignableValue`, one only needs to perform a single assignment, most often from
a handle-template function, instead of repeating the same parameters across multiple class method definitions. -}
class SignableValue (baseType :: Symbol) a where
    signableValueHandle :: SignableValueHandle a
    signableValueListHandle :: SignableValueHandle [a]
    signableValueListHandle = arrayValueHandle (signableValueHandle @baseType @a) Nothing



{-| This typeclass sets one of the Solidity typenames as the default correspondence for the given Haskell type. -}
class (SignableValue baseType a) => SignableValueDefault baseType a | a -> baseType where {}

{-| A version of `signableValueHandle` that uses the stronger `SignableValueDefault` constraint. -}
signableValueDefaultHandle ::
    forall a baseType.
    (SignableValueDefault baseType a) =>
    SignableValueHandle a
signableValueDefaultHandle = signableValueHandle @baseType @a



{-| This newtype is intended to be used as a wrapper for DerivingVia to derive `SignableValue`,
for example:

    @
        data MyPerson = MyPerson
            { pname :: String
            , pwallet :: BS.ByteString
            }
            deriving (Show, Eq, Generic)
            deriving (SignableValue \"Person\") via (
                GenericSignable
                    '[  'SigningFieldModifier "pname"
                            ('Just "name")
                            ('Just (AsBaseType "string" String))
                    ,   'SigningFieldModifier "pwallet"
                            ('Just "wallet")
                            ('Just (AsBaseType "address" BS.ByteString))
                    ]
                    MyPerson
            )
            deriving anytype (SignableValueDefault \"Person\")
    @

    @mods@ is a @DataKinds@ parameter that is used to fine-tune the generated instance, similar to aeson's
`Data.Aeson.Options`, but on a type level instead of the value level. -}
newtype GenericSignable (mods :: [SigningModifier]) a
    = GenericSignable
        { unwrapGenericSignable :: AssertValidation (ValidateModifiers mods (Rep a)) a
        }

instance
    ( Generic s
    , ValidateModifiers mods (Rep s) ~ 'Nothing
    , GSignableRep mods s (Rep s)
    , KnownSymbol baseType
    ) =>
    SignableValue baseType (GenericSignable mods s)
  where
    signableValueHandle = coerce $ genericSignableValueHandle @baseType @mods @s

{-| A datatype for `GenericSignable` modifiers. It's intended to be used at type level with DataKinds.

    Each modifier in the list can be thought of as an assignment of the provided value to a configuration parameter.

    @SigningFieldModifier "haskellFieldName" mbEthName mbSignType@ configures the field, which is referred to
in Haskell by the selector @haskellFieldName@. @mbEthName@ and @mbSignType@ are optional new configuration values:
@'Just smth@ sets the configuration to the specified value, while @'Nothing@ leaves it unchanged.

    The second parameter, @mbEthName@, sets the name of the field in Solidity. By default, the Haskell selector's
name is used.

    The third paremeter, @mbSignType@, is a @DerivingVia@-style wrapper that configures a "signing type". This type
must have a valid `SignableValueDefault` instance, and, obviously, be `Coercible` with the actual field's type. -}
data SigningModifier
    = SigningFieldModifier Symbol (Maybe Symbol) (Maybe Type)

{-| A generic implementation of the signable handle. You can use this function instead of `GenericSignable` to
define your own handle or instance, for example:

    @
        data MyPerson = MyPerson
            { pname :: String
            , pwallet :: BS.ByteString
            }
            deriving (Show, Eq, Generic)

        myPersonSignableValueHandle :: SignableValueHandle MyPerson
        myPersonSignableValueHandle =
            genericSignableValueHandle
                \@\"Person\"
                \@'[ 'SigningFieldModifier "pname"
                        ('Just "name")
                        ('Just (AsBaseType "string" String))
                ,   'SigningFieldModifier "pwallet"
                        ('Just "wallet")
                        ('Just (AsBaseType "address" BS.ByteString))
                ]

        instance SignableValue \"Person\" MyPerson where
            signableValueHandle = myPersonSignableValueHandle

        instance SignableValueDefault \"Person\" MyPerson where {}
    @ -}
genericSignableValueHandle ::
    forall baseType mods s.
    ( Generic s
    , AssertValidation (ValidateModifiers mods (Rep s)) (() :: Constraint)
    , GSignableRep mods s (Rep s)
    , KnownSymbol baseType
    ) =>
    SignableValueHandle s
genericSignableValueHandle =
    structValueHandle (SName @baseType) (gSignableRepMemberHandles @mods @s @(Rep s) from)



{-| This is a newtype wrapper that's indended to be used with `SigningFieldModifier`.

    It sets the Solidity typename to be used for this Haskell type. This allows you to use a `SignableValue`
instance that is different from default, or on types that don't even define a default, such as `Integer` and
`BS.ByteString`. -}
newtype AsBaseType (baseType :: Symbol) a = AsBaseType a

deriving newtype instance (SignableValue baseType a) => SignableValue baseType (AsBaseType otherBaseType a)

instance (SignableValue baseType a) => SignableValueDefault baseType (AsBaseType baseType a)



{-| This is a newtype wrapper that's indended to be used with `SigningFieldModifier`.

    Being a wrapper for an array, it allows you to set a fixed size to the array, as opposed to having the default
option to map the Haskell list to a variable-length array.

    For example, representing a Solidity structure @Rectangle {int256[2] minpos; int256[2] maxpos}@ could be done
like this:

    @
        data Rectangle = Rectangle
            { minpos :: [Integer]
            , maxpos :: [Integer]
            }
            deriving (Generic)
            deriving (SignableValue \"Rectangle\") via (
                GenericSignable
                    '[  'SigningFieldModifier "minpos"
                            'Nothing
                            ('Just (AsSized 2 (AsBaseType "int256" Integer)))
                    ,   'SigningFieldModifier "maxpos"
                            'Nothing
                            ('Just (AsSized 2 (AsBaseType "int256" Integer)))
                    ]
                    Rectangle
            )
    @ -}
newtype AsSized (arrSize :: Nat) a = AsSized [a]

instance (KnownNat arrSize, SignableValue baseType a) => SignableValue baseType (AsSized arrSize a) where
    signableValueHandle =
        coerce $
        arrayValueHandle
            (signableValueHandle @baseType @a)
            (Just $ fromIntegral $ natVal @arrSize undefined)

instance (KnownNat arrSize, SignableValueDefault baseType a) => SignableValueDefault baseType (AsSized arrSize a)



{-| This handle encodes Haskell `Bool` into Solidity's @bool@:

        * `False` is encoded as @`BS.replicate` 32 0@.

        * `True` is encoded as @`BS.replicate` 31 0 <> '\x01'@. -}
boolValueHandle ::
    SignableValueHandle Bool
boolValueHandle = SignableValueHandle
    { signableValueEthType =
        ourEthType
    , signableValueTypeContextBuilder =
        Nothing
    , signableValueTypeContext =
        emptyTypeContext
    , signableValueEmbed = \givenEthType x ->
        case givenEthType of
            EthTypeAtomic AtomicEthTypeBool ->
                pure $ EthValueAtomic x
            _ -> do
                fail $ "Invalid eth type, expected " <> show (withSome ourEthType ethTypeName)
    }
  where
    ourEthType =
        Some $ EthTypeAtomic AtomicEthTypeBool

{-| This handle encodes Haskell `Integer` into a Solidity's integer type: sign-extended to 256 bits
(32 bytes) and serialized in the big-endian order.

    The two parameters define signedness and bit size of the integer. -}
numberValueHandle ::
    AtomicIntegerSignedness ->
    AtomicIntegerBitSize ->
    SignableValueHandle Integer
numberValueHandle signedness bitSize = SignableValueHandle
    { signableValueEthType =
        ourEthType
    , signableValueTypeContextBuilder =
        Nothing
    , signableValueTypeContext =
        emptyTypeContext
    , signableValueEmbed = \givenEthType x ->
        case givenEthType of
            EthTypeAtomic (AtomicEthTypeInteger givenSignedness givenBitSize) ->
                if isValidInteger givenSignedness givenBitSize x
                    then pure $ EthValueAtomic x
                    else fail "Value is out of range"
            _ -> do
                fail $ "Invalid eth type, expected " <> show (withSome ourEthType ethTypeName)
    }
  where
    ourEthType =
        Some $ EthTypeAtomic $ AtomicEthTypeInteger signedness bitSize

{-| This handle encodes Haskell `BS.ByteString` into Solidity's @address@, @bytesN@ or @bytes@.

    The single parameter chooses a subtype:

        * `AtomicBytesSubtypeAddress` allows bytestrings of length 20, and encodes it as an @address@: left-padded with
    zero bytes until the length of 32.

        * @`AtomicBytesSubtypeShortBytes` N@ allows bytestrings with length of exactly @N@, and encodes it as
    a @bytesN@: right-padded with zero bytes until the length of 32. The handle will only make a valid encoding for
    @1 <= N <= 32@.

        * `AtomicBytesSubtypeLongBytes` allows bytestrings of any length and any contents, and encodes it as @bytes@:
    by taking the @keccak256@ of the bytes. -}
bytesValueHandle ::
    AtomicBytesSubtype -> {-^ The subtype of the bytestring. -}
    SignableValueHandle BS.ByteString
bytesValueHandle subtype = SignableValueHandle
    { signableValueEthType =
        ourEthType
    , signableValueTypeContextBuilder =
        Nothing
    , signableValueTypeContext =
        emptyTypeContext
    , signableValueEmbed = \givenEthType x ->
        case givenEthType of
            EthTypeAtomic (AtomicEthTypeBytes givenSubtype) ->
                case givenSubtype of
                    AtomicBytesSubtypeAddress ->
                        if BS.length x == 20
                            then pure $ EthValueAtomic x
                            else fail "Invalid address"
                    AtomicBytesSubtypeShortBytes n ->
                        if BS.length x == n
                            then pure $ EthValueAtomic x
                            else fail "Invalid byte count"
                    AtomicBytesSubtypeLongBytes ->
                        pure $ EthValueAtomic x
            _ -> do
                fail $ "Invalid eth type, expected " <> show (withSome ourEthType ethTypeName)
    }
  where
    ourEthType =
        Some $ EthTypeAtomic $ AtomicEthTypeBytes subtype

{-| This handle encodes Haskell `Text.Text` into Solidity's @string@: by taking the @keccak256@ of the UTF-8 encoding
of the string. -}
stringValueHandle ::
    SignableValueHandle Text.Text
stringValueHandle = SignableValueHandle
    { signableValueEthType =
        ourEthType
    , signableValueTypeContextBuilder =
        Nothing
    , signableValueTypeContext =
        emptyTypeContext
    , signableValueEmbed = \givenEthType x ->
        case givenEthType of
            EthTypeAtomic AtomicEthTypeString ->
                pure $ EthValueAtomic x
            _ -> do
                fail $ "Invalid eth type, expected " <> show (withSome ourEthType ethTypeName)
    }
  where
    ourEthType =
        Some $ EthTypeAtomic AtomicEthTypeString

{-| This handle encodes the array type.

    The first parameter is the handle for the underlying type, it will be used to encode each element of the array.
The second parameter is the size of the array: a `Just`-number for a fixed-size array (e.g. @address[32]@) or
`Nothing` for a variable-length array (e.g. @string[]@).

    The encoding in both cases is the `keccak256` of the concatenation of the content's encodings.

    If the array is fixed-sized, then the actual value's length will be validated upon encoding. -}
arrayValueHandle ::
    SignableValueHandle a -> {-^ Underlying type's handle. -}
    Maybe Int -> {-^ Array size. -}
    SignableValueHandle [a]
arrayValueHandle elemHandle mbSize = SignableValueHandle
    { signableValueEthType =
        ourEthType
    , signableValueTypeContextBuilder =
        signableValueTypeContextBuilder elemHandle
    , signableValueTypeContext =
        signableValueTypeContext elemHandle
    , signableValueEmbed = \givenEthType xs ->
        case givenEthType of
            EthTypeArray givenElemType givenMbSize -> do
                case givenMbSize of
                    Nothing -> pure ()
                    Just givenSize ->
                        if length xs == givenSize
                            then pure ()
                            else fail "Invalid array size"
                ys <- forM (zip [0 :: Int ..] xs) $ \(i, x) ->
                    withEmbedLocationArrayElement i $
                        signableValueEmbed elemHandle givenElemType x
                pure $ EthValueArray ys
            _ -> do
                fail $ "Invalid eth type, expected " <> show (withSome ourEthType ethTypeName)
    }
  where
    ourEthType =
        withSome (signableValueEthType elemHandle) $ \elemType ->
            Some $ EthTypeArray elemType mbSize



{-| This handle encodes a struct type.

    The encoding of a struct is @keccak256@ of the concatenation of the struct's typehash and each of its value's
encoding in their definition order.

    An example usage of this function:

    @
        data MyPerson = MyPerson
            { pname :: String
            , pwallet :: BS.ByteString
            }
            deriving (Show, Eq, Generic)

        myPersonSignableValueHandle :: SignableValueHandle MyPerson
        myPersonSignableValueHandle =
            structValueHandle
                (SName \@\"Person\")
                [   SignableStructMemberHandle "name" pname (signableValueHandle \@"string")
                ,   SignableStructMemberHandle "wallet" pwallet (signableValueHandle \@"address")
                ]
    @ -}
structValueHandle ::
    forall s structName.
    SName structName ->
        {-^ The name of the struct. Use @-XTypeApplications@ to declare constants here, e.g. @SName \@\"Person\"@. -}
    [SignableStructMemberHandle s] -> {-^ The list of the struct's member handles. -}
    SignableValueHandle s
structValueHandle structSName structMembers = SignableValueHandle
    { signableValueEthType =
        Some $ EthTypeStruct structSName
    , signableValueTypeContextBuilder =
        Just ourTypeContextBuilder
    , signableValueTypeContext =
        executeTypeContextBuilder ourTypeContextBuilder
    , signableValueEmbed = \givenEthType structInstance ->
        case givenEthType of
            EthTypeStruct givenStructSName -> do
                givenStructDef <- getStructDef givenStructSName
                embeddedMembers <-
                    htraverse
                        (\(MemberDef givenMemberEthType givenMemberName) ->
                            withEmbedLocationStructMember givenMemberName $
                                case Map.lookup givenMemberName ourMemberEmbedderMap of
                                    Nothing -> do
                                        fail "Unknown struct field"
                                    Just embedder -> do
                                        runStructMemberEmbedder embedder givenMemberEthType structInstance
                        )
                        givenStructDef
                pure $ EthValueStruct embeddedMembers
            _ -> do
                fail $ "Invalid eth type, expected " <> show (fromSName structSName)
    }
  where
    ourStructDefinition =
        hsequenceSome $
        map
            (\(SignableStructMemberHandle memberName _ memberValueHandle) ->
                withSome (signableValueEthType memberValueHandle) $ \memberEthType ->
                    Some $ MemberDef memberEthType memberName
            )
            structMembers
    ourTypeContextBuilder =
        TypeContextBuilder
            (Some structSName)
            ourStructDefinition
            (mapMaybe
                (\(SignableStructMemberHandle _ _ memberValueHandle) ->
                    signableValueTypeContextBuilder memberValueHandle
                )
                structMembers
            )
    ourMemberEmbedderMap :: Map.Map Text.Text (StructMemberEmbedder s)
    ourMemberEmbedderMap =
        Map.fromListWithKey errOnDuplicate $
        map makeMemberEmbedder $
        structMembers
    errOnDuplicate :: Text.Text -> a -> a -> a
    errOnDuplicate k _ _ =
        error $ "Duplicate struct member " <> show k
    makeMemberEmbedder :: SignableStructMemberHandle s -> (Text.Text, StructMemberEmbedder s)
    makeMemberEmbedder (SignableStructMemberHandle memberName memberAccessor memberValueHandle) =
        let memberEmbedder =
                StructMemberEmbedder $ \givenEthType structInstance ->
                    signableValueEmbed
                        memberValueHandle
                        givenEthType
                        (memberAccessor structInstance)
        in
        (memberName, memberEmbedder)

{-| A handle for a specific field of a struct, to be used in `structValueHandle`. -}
data SignableStructMemberHandle s =
    forall a.
    SignableStructMemberHandle
        {-| The name of the field in Solidity. -}
        Text.Text
        {-| The accessor function (selector) of the field. -}
        (s -> a)
        {-| The encoding handle for the Haskell type of the field. -}
        (SignableValueHandle a)

newtype StructMemberEmbedder s = StructMemberEmbedder
    { runStructMemberEmbedder ::
        forall ctx rep.
        EthType rep ->
        s ->
        Embed ctx (EthValue ctx rep)
    }



{-| The handle that defines how a given Haskell value shall be encoded into a Solidity-compatible representation.

    Normally, you should use one of the pre-defined handle factories, and possibly `contramap` or
`contramapSignableEither` them onto your type.

    `EthType` is a declaration of the Solidity type. Atomic types are declared completely, while struct types are
declared only by a reference into a "type context". This allows multiple struct types to recursively refer to each
other, a feature that's explicitly supported by EIP-712.

    The encoding is done in two phases:

        * Firstly, we collect all struct definitions that are used in the root value to create the `TypeContext`.

        * Then, we actually traverse the given object, while resolving the struct-referencing `EthType`s with
    the constructed `TypeContext`.

    When building the type context, recursive struct definitions can lead to a value recursion in Haskell. For this
reason, the primary source for a single handle is a value of `TypeContextBuilder`, which is a single struct definition
followed by a list of its dependencies. When traversing the `TypeContextBuilder`s, we can detect this kind of recursion
by an explicit check and thus avoid getting stuck in an infinite loop when constructing the singular
`TypeContext` object.

    The Solidity value representation uses quite a bit of type magic to ensure consistency, but it's still not enough
to prove that a struct within a `TypeContext` will exactly match the `TypeContextBuilder` that it came from.
For this reason, `signableValueEmbed` takes a separate `EthType`, which may be different from the one given in
`signableValueEthType`, and is allowed to report a mismatch between the Haskell type and the Solidity representation
though the `MonadFail` instance of the `Embed` monad. -}
data SignableValueHandle a
    = SignableValueHandle
        {-| The Solidity type that the values of this Haskell type /should/ be mapped to. -}
        { signableValueEthType ::
            Some EthType
        {-| A "builder" for the type context. If this handle corresponds to a struct, this will be `Just` the
        definition of this struct and a list of its members' dependencies. Otherwise, this will be `Nothing`. -}
        , signableValueTypeContextBuilder ::
            Maybe TypeContextBuilder
        {-| The cached result of calling `executeTypeContextBuilder` on this handle's builder. -}
        , signableValueTypeContext ::
            Some TypeContext
        {-| The encoder function. It's given a `TypeContext` as part of the `Embed` monad, an `EthType` and a Haskell
        value, and should either encode the given value according to the given `EthType`, or report a failure
        through the monad. -}
        , signableValueEmbed ::
            forall ctx rep. EthType rep -> a -> Embed ctx (EthValue ctx rep)
        }

instance Contravariant SignableValueHandle where
    contramap into handle =
        handle
            { signableValueEmbed = \givenEthType x ->
                signableValueEmbed handle givenEthType (into x)
            }

{-| A generalization of @`contramap` \@SignableValueHandle@ that allows you to perform run-time checks and
fail gracefully by returning a `Left`. An error produced in this way will be annotated with a JSONPath-like location
in the object where it happened.

    If the conversion is total, you can use `contramap` instead. -}
contramapSignableEither ::
    (a -> Either String b) ->
    SignableValueHandle b ->
    SignableValueHandle a
contramapSignableEither intoEither handle =
    handle
        { signableValueEmbed = \givenEthType x -> do
            case intoEither x of
                Left e -> fail e
                Right y -> signableValueEmbed handle givenEthType y
        }

{-| A builder of a `TypeContext`. This structure is specifically designed to allow value recursion, so that
a definition like this would be legal:

    @
        data FoobarTree = FoobarTree
            { stData :: Foobar
            , stChildren :: [FoobarTree]
            }

        foobarTreeTCB :: TypeContextBuilder
        foobarTreeTCB =
            TypeContextBuilder
                (Some $ SName \@\"FoobarTree\")
                (Some $
                    MemberDef
                        (EthTypeStruct (SName \@\"Foobar\"))
                        "data"
                  :/
                    MemberDef
                        (EthTypeArray (EthTypeStruct (SName \@\"FoobarTree\")) Nothing)
                        "children"
                  :/
                    HEnd
                )
                [ foobarTCB
                , foobarTreeTCB -- value recursion
                ]
    @ -}
data TypeContextBuilder
    = TypeContextBuilder
        (Some SName) {-^ This struct's Solidity name. -}
        (Some StructDef) {-^ This struct's Solidity definition. -}
        [TypeContextBuilder] {-^ This struct's members' dependencies. -}

{-| Fold a `TypeContextBuilder` into a `TypeContext`. -}
executeTypeContextBuilder :: TypeContextBuilder -> Some TypeContext
executeTypeContextBuilder = visit mempty
  where
    visit :: Some TypeContext -> TypeContextBuilder -> Some TypeContext
    visit accum (TypeContextBuilder (Some sname) (Some sdef) deps)
        | isDefinitionPresent accum sname sdef =
            accum
        | otherwise =
            foldl'
                visit
                (accum <> singletonTypeContext sname sdef)
                deps
    isDefinitionPresent :: Some TypeContext -> SName typename -> StructDef reps -> Bool
    isDefinitionPresent (Some tc) sname sdef =
        case lookupHMap sname (typeContextMap tc) of
            Just oldSDef
                | Just _ <- testEquality oldSDef sdef ->
                    True
            _ -> False



{-| A monad that is used for the main phase of the value encoding. Its structure can be represented by this stack:

    @
        Embed ctx

      is-semantically-equivalent-to

        ReaderT (TypeContext ctx) -- The type context that all of the struct-referring `EthType`s point to
      .
        ReaderT String -- The current position in the value, for error messages
      .
        ExceptT String -- Type mismatch errors
    @ -}
newtype Embed ctx a = Embed
    { withEmbed ::
        forall r.
        TypeContext ctx ->
        ShowS ->
        (String -> r) ->
        (a -> r) ->
        r
    }
    deriving (Functor)

{-| The driver of the `Embed` actions. -}
runEmbed :: TypeContext ctx -> Embed ctx a -> Either String a
runEmbed tc enc = withEmbed enc tc (showString "$") Left Right

instance Applicative (Embed ctx) where
    pure x = Embed $ \_ _ _ k -> k x
    mf <*> mx = Embed $ \tc pos t k ->
        withEmbed mf tc pos t $ \f ->
            withEmbed mx tc pos t $ \x ->
                k (f x)
    liftA2 f mx my = Embed $ \tc pos t k ->
        withEmbed mx tc pos t $ \x ->
            withEmbed my tc pos t $ \y ->
                k (f x y)

instance Monad (Embed ctx) where
    mx >>= sel = Embed $ \tc pos t k ->
        withEmbed mx tc pos t $ \x ->
            withEmbed (sel x) tc pos t k

instance MonadFail (Embed ctx) where
    fail msg = Embed $ \_ pos t _ ->
        t (pos $ ": " ++ msg)

{-| @local@-like function that appends the given string to the current location. -}
withEmbedLocation :: String -> Embed ctx a -> Embed ctx a
withEmbedLocation str enc =
    Embed $ \tc pos t k ->
        withEmbed enc tc (pos . showString str) t k

{-| `withEmbedLocation` specialization that appends an array index. -}
withEmbedLocationArrayElement :: Int -> Embed ctx a -> Embed ctx a
withEmbedLocationArrayElement i =
    withEmbedLocation $ "[" <> show i <> "]"

{-| `withEmbedLocation` specialization that appends a member access. -}
withEmbedLocationStructMember :: Text.Text -> Embed ctx a -> Embed ctx a
withEmbedLocationStructMember mname =
    withEmbedLocation $ "." <> Text.unpack mname

{-| Retrieve the current type context. -}
askTypeContext :: Embed ctx (TypeContext ctx)
askTypeContext = Embed $ \tc _ _ k ->
    k tc

{-| Lookup a struct definition by its name in the current type context. -}
getStructDef :: SName typename -> Embed ctx (StructDef (Lookup typename ctx))
getStructDef sname = Embed $ \tc _ _ k ->
    withStructDef tc sname k



{-| Helper typeclass for the generic implementation of `SignableValue`.

    The role of `GSignableRep` is to collect information about the fields of the structure. This list of fields
is then used as a parameter to `structValueHandle` to make the complete handle.

    Type parameter @s@ stands for the original Haskell type, which the handle is being built for; and @g@ is
a @GHC.Generics@-representation of a part within the original value. -}
class GSignableRep (mods :: [SigningModifier]) s g where
    gSignableRepMemberHandles ::
        (s -> g ()) ->
        [SignableStructMemberHandle s]

instance (GSignableRep mods s g) => GSignableRep mods s (D1 md g) where
    gSignableRepMemberHandles fn =
        gSignableRepMemberHandles @mods @s @g (unM1 #. fn)

instance (GSignableRep mods s g) => GSignableRep mods s (C1 mc g) where
    gSignableRepMemberHandles fn =
        gSignableRepMemberHandles @mods @s @g (unM1 #. fn)

instance (GSignableRep mods s ga, GSignableRep mods s gb) => GSignableRep mods s (ga :*: gb) where
    gSignableRepMemberHandles fn =
        gSignableRepMemberHandles @mods @s @ga (gfst . fn)
      <>
        gSignableRepMemberHandles @mods @s @gb (gsnd . fn)
      where
        gfst (x :*: _) = x
        gsnd (_ :*: y) = y

instance
    ( SelectedFieldName (ModifiedFieldName hsel mods) hsel ethFieldName
    , SelectedFieldSignType (ModifiedFieldType hsel mods) a baseType signType
    ) =>
    GSignableRep mods s (S1 ('MetaSel ('Just hsel) mssu msss msds) (K1 i a))
  where
    gSignableRepMemberHandles fn =
        [ SignableStructMemberHandle
            (symbolText @ethFieldName)
            ((unK1 . unM1) #. fn)
            (coerce $ signableValueHandle @baseType @signType)
        ]



class (KnownSymbol ethName) => SelectedFieldName m hsel ethName | m hsel -> ethName where {}

instance (KnownSymbol ethName) => SelectedFieldName 'Nothing ethName ethName

instance (KnownSymbol ethName) => SelectedFieldName ('Just ethName) hsel ethName



class (Coercible a s, SignableValue baseType s) => SelectedFieldSignType m a baseType s | m a -> baseType s where {}

instance (SignableValueDefault baseType a) => SelectedFieldSignType 'Nothing a baseType a

instance (Coercible a s, SignableValueDefault baseType s) => SelectedFieldSignType ('Just s) a baseType s



(#.) :: Coercible b c => (b -> c) -> (a -> b) -> a -> c
_ #. f = coerce f



{-| @ModifiedFieldName hsel mods@ is the Solidity field name that @mods@ assign to selector @hsel@. -}
type family ModifiedFieldName hsel mods where
    ModifiedFieldName hsel ('SigningFieldModifier hsel mbName mbType ': otherMods) =
        SafeUnify mbName (ModifiedFieldName hsel otherMods)
    ModifiedFieldName hsel (_ ': otherMods) =
        ModifiedFieldName hsel otherMods
    ModifiedFieldName hsel '[] =
        'Nothing

{-| @ModifiedFieldType hsel mods@ is the wrapper type that @mods@ assign to selector @hsel@. -}
type family ModifiedFieldType hsel mods where
    ModifiedFieldType hsel ('SigningFieldModifier hsel mbName mbType ': otherMods) =
        SafeUnify mbType (ModifiedFieldType hsel otherMods)
    ModifiedFieldType hsel (_ ': otherMods) =
        ModifiedFieldType hsel otherMods
    ModifiedFieldType hsel '[] =
        'Nothing

type family SafeUnify m1 m2 where
    SafeUnify m1 'Nothing = m1
    SafeUnify 'Nothing m2 = m2
    SafeUnify ('Just x) ('Just x) = 'Just x

{-| @ValidateModifiers mods rep@ checks that the modifier list @mods@:

    * is internally consistent: it does not assign different names or different types to the same field, and

    * is applicable to the generic representation @rep@: it does not assign to fields that are not actually present
in the structure.

    If the checks pass, it reduces to @'Nothing@, otherwise it's @'Just@ with the description of the failure
as a type-level `ErrorMessage`. -}
type family ValidateModifiers mods rep where
    ValidateModifiers (mod ': rest) rep =
        ValidateModifierApplicable mod rep
      <|>
        ValidateModifierCompatible mod rest
      <|>
        ValidateModifiers rest rep
    ValidateModifiers '[] rep =
        'Nothing

type family ValidateModifierCompatible mod others where
    ValidateModifierCompatible mod (cand ': rest) =
        If
            (AreFieldModifiersCompatible mod cand)
            (ValidateModifierCompatible mod rest)
            ('Just
                (   'Text "Conflicting modifiers:" ':$$:
                    'Text "    " ':<>: 'ShowType mod ':$$:
                    'Text "    " ':<>: 'ShowType cand
                )
            )
    ValidateModifierCompatible mod '[] =
        'Nothing

type family AreFieldModifiersCompatible mod1 mod2 where
    AreFieldModifiersCompatible ('SigningFieldModifier hsel1 t1 n1) ('SigningFieldModifier hsel2 t2 n2) =
        Not (hsel1 == hsel2) || (AreMaybesCompatible t1 t2 && AreMaybesCompatible n1 n2)

type family AreMaybesCompatible a b where
    AreMaybesCompatible 'Nothing _ = 'True
    AreMaybesCompatible _ 'Nothing = 'True
    AreMaybesCompatible ('Just x) ('Just y) = x == y

type family ValidateModifierApplicable mod rep where
    ValidateModifierApplicable mod rep =
        If
            (IsModifierApplicable mod rep)
            'Nothing
            ('Just
                (   'Text "Modifier not applicable:" ':$$:
                    'Text "    " ':<>: 'ShowType mod
                )
            )

type family IsModifierApplicable mod rep where
    IsModifierApplicable ('SigningFieldModifier hsel _ _) (S1 ('MetaSel ('Just hsel) _ _ _) g) =
        'True
    IsModifierApplicable mod (ga :*: gb) =
        IsModifierApplicable mod ga || IsModifierApplicable mod gb
    IsModifierApplicable mod (C1 _ g) =
        IsModifierApplicable mod g
    IsModifierApplicable mod (D1 _ g) =
        IsModifierApplicable mod g
    IsModifierApplicable mod _ =
        'False

type family m1 <|> m2 where
    'Just x <|> m2 = 'Just x
    'Nothing <|> m2 = m2

{-| @AssertValidation mbx r@ reduces to @r@ if @mbx ~ 'Nothing@, otherwise it's @TypeError@.

    It's intended to be used with `ValidateModifiers` type family, in the form
@AssertValidation (ValidateModifiers mods rep) r@.

    The reason it's split into a separate type family is to allow a form of validation that /does not/ throw
a type error on failure: @ValidateModifiers mods rep ~ 'Nothing@. This is sometimes used in conjunction with the
"throwing" version to avoid duplicate error messages. -}
type family AssertValidation mbx r where
    AssertValidation 'Nothing r = r
    AssertValidation ('Just msg) r = TypeError msg



instance (SignableValue baseType a) => SignableValue baseType [a] where
    signableValueHandle = signableValueListHandle @baseType @a

instance SignableValue "bool" Bool where
    signableValueHandle =
        boolValueHandle

$(do
    let signedInstance tt (b :: Integer) = do
            [d|
                instance
                    SignableValue
                        $(pure $ TH.LitT $ TH.StrTyLit $ "int" <> show b)
                        $(pure $ TH.ConT tt)
                  where
                    signableValueHandle =
                        contramap toInteger $
                        numberValueHandle
                            AtomicIntegerSigned
                            $(pure $ TH.LitE $ TH.IntegerL b)
              |]
    let unsignedInstance tt (b :: Integer) = do
            [d|
                instance
                    SignableValue
                        $(pure $ TH.LitT $ TH.StrTyLit $ "uint" <> show b)
                        $(pure $ TH.ConT tt)
                  where
                    signableValueHandle =
                        contramap toInteger $
                        numberValueHandle
                            AtomicIntegerUnsigned
                            $(pure $ TH.LitE $ TH.IntegerL b)
              |]
    fmap (concat . concat) $ sequence
        [ traverse (signedInstance ''Integer) [8, 16 .. 256]
        , traverse (unsignedInstance ''Integer) [8, 16 .. 256]
        , traverse (unsignedInstance ''Natural) [8, 16 .. 256]
        , traverse (signedInstance ''Int) [8, 16 .. 64]
        , traverse (unsignedInstance ''Word) [8, 16 .. 64]
        , traverse (signedInstance ''Int8) [8]
        , traverse (signedInstance ''Int16) [16]
        , traverse (signedInstance ''Int32) [32]
        , traverse (signedInstance ''Int64) [64]
        , traverse (unsignedInstance ''Word8) [8]
        , traverse (unsignedInstance ''Word16) [16]
        , traverse (unsignedInstance ''Word32) [32]
        , traverse (unsignedInstance ''Word64) [64]
        ]
 )

instance SignableValue "address" BS.ByteString where
    signableValueHandle =
        bytesValueHandle AtomicBytesSubtypeAddress

$(do
    let bytesInstance b = do
            [d|
                instance SignableValue $(pure $ TH.LitT $ TH.StrTyLit $ "bytes" <> show b) BS.ByteString where
                    signableValueHandle =
                        bytesValueHandle
                            (AtomicBytesSubtypeShortBytes $(pure $ TH.LitE $ TH.IntegerL b))
              |]
    fmap concat $ traverse bytesInstance [1 .. 32]
 )

instance SignableValue "bytes" BS.ByteString where
    signableValueHandle =
        bytesValueHandle AtomicBytesSubtypeLongBytes

instance SignableValue "string" BS.ByteString where
    signableValueHandle =
        contramapSignableEither (
            either (Left . show) Right .
            Text.Encoding.decodeUtf8'
        ) $
        stringValueHandle

instance SignableValue "string" Text.Text where
    signableValueHandle =
        stringValueHandle

instance SignableValue "string" Char where
    signableValueHandle =
        contramap Text.singleton $
        stringValueHandle
    signableValueListHandle =
        contramap Text.pack $
        stringValueHandle

instance (SignableValueDefault baseType a) => SignableValueDefault baseType [a]

instance SignableValueDefault "bool" Bool

instance SignableValueDefault "int8" Int8

instance SignableValueDefault "int16" Int16

instance SignableValueDefault "int32" Int32

instance SignableValueDefault "int64" Int64

instance SignableValueDefault "uint8" Word8

instance SignableValueDefault "uint16" Word16

instance SignableValueDefault "uint32" Word32

instance SignableValueDefault "uint64" Word64

instance SignableValueDefault "string" Text.Text

instance SignableValueDefault "string" Char
