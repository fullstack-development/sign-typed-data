{-| This modules defines a runtime representation of strictly typed Solidity values.

    This representation is composed of three parts:

        * A `TypeContext` gives a mapping from type names to their declarations for custom types (structs).

        * An `EthType` gives a type definition. Custom types here are only referred by name, instead of being
    contained in their entirety, so the exact representation of a value will depend on both a `TypeContext` and
    an `EthValue`.

        * An `EthValue` is a type family that holds a Solidity object of a given Solidity type.

    A mediator between the `EthType` and `EthValue` is `EthRepr`, which is a stripped type-level version of
`EthType`. It contains only the information relevant to choosing the appropriate Haskell type for representation
within an `EthValue`. And just like with `EthType`, custom types are only referred by their names in `EthRepr` too,
and similarly require an addition of a type context (on a type level) to resolve fully. -}
module Data.SignTyped.Structure
    {-* Type definitions -}
    ( EthType (..)
    , AtomicEthType (..)
    , AtomicIntegerSignedness (..)
    , AtomicIntegerBitSize
    , AtomicBytesSubtype (..)
    , ethTypeName
    , EthRepr (..)

    {-* Typed values -}
    , EthValue (..)
    , defaultEthValue
    , eqEthValue
    , compareEthValue

    {-* Struct definitions -}
    , StructDef
    , MemberDef (..)

    {-* Type context -}
    , TypeContext (..)
    , emptyTypeContext
    , singletonTypeContext
    , lookupStructDef
    , withStructDef

    {-* Printing for debugging -}
    , showsEthValue
    , showsEthStructDef
    , showsTypeContext

    {-* Unmarked Solidity objects for testing -}
    , SomeEthValue (..)

    {-* Validating type invariants -}
    , validateEthValue
    , validateEthValueWorker
    , isValidInteger
    )
where

import Data.Kind
import Data.SignTyped.Type.HList
import Data.SignTyped.Type.HMap
import Data.SignTyped.Type.SName
import Data.SignTyped.Type.Some
import Data.SignTyped.Util
import Data.Type.Equality
import qualified Data.ByteString as BS
import qualified Data.Text as Text



{-| A GADT defining a Solidity type. The type marker is an `EthRepr` selecting the appropriate Haskell representation
for the given Solidity type. -}
data EthType rep where
    {-| Atomic types are defined fully in-place.

        They each have specific encodings and JSON mappings. -}
    EthTypeAtomic ::
        AtomicEthType t -> {-^ An in-place definition for the type. -}
        EthType ('EthReprAtomic t)
    {-| Array types are composed of a base type and an optional size.

        The encoding is @keccak256@ of the concatenation of the elements' encodings.

        In JSON, an array is represented as a JSON array. -}
    EthTypeArray ::
        EthType rep -> {-^ Base (element) type. -}
        Maybe Int ->
            {-^ Array size. If present, the value must have this exact number of elements. Otherwise, any number
            of elements is permitted. -}
        EthType ('EthReprArray rep)
    {-| Structs are kept as references, to be fully resolved later with a `TypeContext`.

        The encoding is @keccak256@ of a concatenation of the struct's type hash (dependent on its definition in
    the type context) and the encodings of its member values.

        In JSON, a struct is represented by a JSON object, with keys corresponding to the struct members' names. -}
    EthTypeStruct ::
        SName typename -> {-^ The name of the struct, as a singleton. -}
        EthType ('EthReprStruct typename)

deriving instance Show (EthType rep)

instance TestEquality EthType where
    testEquality (EthTypeAtomic at1) (EthTypeAtomic at2)
        | Just Refl <- testEquality at1 at2 =
            Just Refl
    testEquality (EthTypeArray elemType1 mbSize1) (EthTypeArray elemType2 mbSize2)
        | Just Refl <- testEquality elemType1 elemType2
        , mbSize1 == mbSize2 =
            Just Refl
    testEquality (EthTypeStruct sname1) (EthTypeStruct sname2)
        | Just Refl <- testEquality sname1 sname2 =
            Just Refl
    testEquality _ _ =
        Nothing

{-| A GADT that gives specific definitions for atomic types. -}
data AtomicEthType t where
    {-| Represented by Haskell `Bool`: Solidity @bool@.

        A @bool@ is encoded as a @uint256@, with `False` and `True` corresponding respectively to 0 and 1.

        In JSON, the literals @false@ and @true@ are used. -}
    AtomicEthTypeBool :: AtomicEthType Bool
    {-| Represented by Haskell `Integer`: Solidity @intN@ and @uintN@.

        When a number is encoded, it is sign-extended to 256 bits (32 bytes) and written out in big-endian order.

        When serialized into JSON, the representation is dependent on the value's magnitude: if it is small enough
    to fit in the exact-integer-range of IEEE Double (which JSON numbers are usually implemented as), then the number
    is written as-is. Otherwise, it is converted to hexadecimal and written as a JSON string with preceding @0x@.

        On input from JSON, either form for any magnitude is allowed. Since Haskell's @aeson@ always parses numbers
    precisely, it does not suffer from Double's limitations. -}
    AtomicEthTypeInteger :: AtomicIntegerSignedness -> AtomicIntegerBitSize -> AtomicEthType Integer
    {-| Represented by Haskell `BS.ByteString`: Solidity @address@, @bytesN@ and @bytes@.

        The single parameter further distinguishes between these variants, controls the way values are
    encoded for signing and mapped to JSON for sign requests, and can also impose restrictions on the value. -}
    AtomicEthTypeBytes :: AtomicBytesSubtype -> AtomicEthType BS.ByteString
    {-| Represented by Haskell `Text.Text`: Solidity @string@.

        The encoding is @keccak256@ of the UTF-8 encoding of the string.

        When serialized into JSON, the string is written out in literal text, unless it starts with @0x@ or @0X@ or
    contains an embedded zero, in which case it's encoded in UTF-8 and written in hexadecimal, like @bytes@. -}
    AtomicEthTypeString :: AtomicEthType Text.Text

deriving instance Show (AtomicEthType t)

instance TestEquality AtomicEthType where
    testEquality AtomicEthTypeBool AtomicEthTypeBool =
        Just Refl
    testEquality (AtomicEthTypeInteger sign1 bitSize1) (AtomicEthTypeInteger sign2 bitSize2)
        | sign1 == sign2 && bitSize1 == bitSize2 =
            Just Refl
    testEquality (AtomicEthTypeBytes subtype1) (AtomicEthTypeBytes subtype2)
        | subtype1 == subtype2 =
            Just Refl
    testEquality AtomicEthTypeString AtomicEthTypeString =
        Just Refl
    testEquality _ _ =
        Nothing

type AtomicIntegerBitSize = Int

{-| Chooses between @uintN@ and @intN@ types. -}
data AtomicIntegerSignedness = AtomicIntegerUnsigned | AtomicIntegerSigned
    deriving (Show, Eq)

{-| Deconstructs an `AtomicIntegerSignedness` value, similar to `maybe`, `either` and `Data.Bool.bool`. -}
matchAtomicIntegerSignedness ::
    a -> {-^ Value to produce on `AtomicIntegerUnsigned` -}
    a -> {-^ Value to produce on `AtomicIntegerSigned` -}
    AtomicIntegerSignedness ->
    a
matchAtomicIntegerSignedness u s = \case
    AtomicIntegerUnsigned -> u
    AtomicIntegerSigned -> s

{-| A subcategory of a bytes-represented type. -}
data AtomicBytesSubtype
    {-| Solidity @address@.

        The bytestring's length must be exactly 20.

        Upon encoding, the bytes are left-padded until the length of 32.

        In JSON, the addresses are represented as hexadecimal strings with the mixed-case checksumming (ERC-55).
    The casing is strictly verified on input. -}
    = AtomicBytesSubtypeAddress
    {-| Solidity @bytesN@, with the N given in the single parameter.

        The bytestring's length must be exactly N.

        Upon encoding, the bytes are right-padded until the length of 32.

        In JSON, the bytes are written out as a hexadecimal string. The input is case-insensitive. On output,
    lowercase digits are produced. -}
    | AtomicBytesSubtypeShortBytes Int
    {-| Solidity @bytes@.

        There are no restrictions on the bytestring.

        The encoding is @keccak256@ of the bytes.

        In JSON, the bytes are written out as a hexadecimal string. The input is case-insensitive. On output,
    lowercase digits are produced. -}
    | AtomicBytesSubtypeLongBytes
    deriving (Show, Eq)

{-| Converts the marked type definition into a simple string.

    For the inverse conversion, see `Data.SignTyped.Json.parseUnmarkedEthType`.-}
ethTypeName :: EthType rep -> Text.Text
ethTypeName = \case
    EthTypeAtomic AtomicEthTypeBool ->
        "bool"
    EthTypeAtomic (AtomicEthTypeInteger signedness bitSize) ->
        matchAtomicIntegerSignedness "uint" "int" signedness <>
        Text.pack (show bitSize)
    EthTypeAtomic (AtomicEthTypeBytes AtomicBytesSubtypeAddress) ->
        "address"
    EthTypeAtomic (AtomicEthTypeBytes (AtomicBytesSubtypeShortBytes n)) ->
        "bytes" <> Text.pack (show n)
    EthTypeAtomic (AtomicEthTypeBytes AtomicBytesSubtypeLongBytes) ->
        "bytes"
    EthTypeAtomic AtomicEthTypeString ->
        "string"
    EthTypeArray elemType Nothing ->
        ethTypeName elemType <> "[]"
    EthTypeArray elemType (Just size) ->
        ethTypeName elemType <> "[" <> Text.pack (show size) <> "]"
    EthTypeStruct sname ->
        fromSName sname

{-| A kind for selecting a Haskell representation for a Solidity object. -}
data EthRepr
    = EthReprAtomic Type
    | EthReprArray EthRepr
    | EthReprStruct Symbol



{-| Haskell representation of a Solidity object of the given type.

    The first parameter matches the representation with a `TypeContext`, and the second with an `EthType`.

    To inspect an arbitrary `EthValue`, you will need its corresponding `TypeContext` and `EthType`. As examples
of processing arbitrary objects using this type information, you can look at the sources of `defaultEthValue`,
`Data.SignTyped.Json.ethValueToEncoding`, `Data.SignTyped.Json.ethValueParseJSON`,
`Data.SignTyped.Hashing.hashEthValue` and other functions from this package. -}
data family EthValue (ctx :: TMap Symbol [EthRepr]) (rep :: EthRepr)

{-| Atomic values are represented directly by specific Haskell types.

    When given an @EthTypeAtomic name atomType@, use pattern-matching on the @atomType@ to determine the exact
representation type. -}
newtype instance EthValue ctx ('EthReprAtomic t)
    = EthValueAtomic
        { unwrapEthValueAtomic :: t
        }

{-| Arrays are represented as Haskell lists.

    When given an @EthTypeArray baseType mbSize@, inspect @baseType@ to determine the representation of
the elements. -}
newtype instance EthValue ctx ('EthReprArray rep)
    = EthValueArray
        { unwrapEthValueArray :: [EthValue ctx rep]
        }

{-| Structs are represented as `HList`s of members.

    When given an @EthTypeStruct sname@, use `lookupStructDef` or `withStructDef` to resolve the struct's name into
its members `HList`, after which you can `hzip`, `hzipWith` or manually pattern-match the two `HList`s and retrieve
the representation of each member from its corresponding `MemberDef`. -}
newtype instance EthValue ctx ('EthReprStruct typename)
    = EthValueStruct
        { unwrapEthValueStruct :: HList (EthValue ctx) (Lookup typename ctx)
        }

{-| Generate a default value for a given type:

    * @bool@ defaults to `False`.

    * Integer types default to 0.

    * Fixed-length strings (@address@ and @bytesN@) are filled with zero bytes.

    * Variable-length strings (@bytes@ and @string@) are defaulted to the empty string.

    * Fixed-length arrays are filled with the copies of the base type's default value.

    * Variable-length arrays are defaulted empty.

    * Structs have each of their member defaulted. -}
defaultEthValue ::
    TypeContext ctx ->
    EthType rep ->
    EthValue ctx rep
defaultEthValue tc = \case
    EthTypeAtomic AtomicEthTypeBool ->
        EthValueAtomic False
    EthTypeAtomic (AtomicEthTypeInteger _ _) ->
        EthValueAtomic 0
    EthTypeAtomic (AtomicEthTypeBytes AtomicBytesSubtypeAddress) ->
        EthValueAtomic "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0"
    EthTypeAtomic (AtomicEthTypeBytes (AtomicBytesSubtypeShortBytes size)) ->
        EthValueAtomic $ BS.replicate size 0
    EthTypeAtomic (AtomicEthTypeBytes AtomicBytesSubtypeLongBytes) ->
        EthValueAtomic ""
    EthTypeAtomic AtomicEthTypeString ->
        EthValueAtomic ""
    EthTypeArray _ Nothing ->
        EthValueArray []
    EthTypeArray elemType (Just size) ->
        EthValueArray $ replicate size (defaultEthValue tc elemType)
    EthTypeStruct sname ->
        withStructDef tc sname $ \memDefs ->
            EthValueStruct $
                hmap
                    (\(MemberDef mtype _) -> defaultEthValue tc mtype)
                    memDefs

{-| Checks equality of two Solidity objects of the same type.

    Since inspecting `EthValue`s requires type information to be passed along with the value, we cannot do this in
the standard v`(==)` operator. -}
eqEthValue ::
    TypeContext ctx ->
    EthType rep ->
    EthValue ctx rep ->
    EthValue ctx rep ->
    Bool
eqEthValue tc ethType v1 v2 =
    compareEthValue tc ethType v1 v2 == EQ


{-| Compares the ordering of two Solidity objects of the same type.

    Since inspecting `EthValue`s requires type information to be passed along with the value, we cannot do this in
the standard `compare` operator. -}
compareEthValue ::
    TypeContext ctx ->
    EthType rep ->
    EthValue ctx rep ->
    EthValue ctx rep ->
    Ordering
compareEthValue tc = \case
    EthTypeAtomic atomicEthType ->
        \(EthValueAtomic x1) (EthValueAtomic x2) ->
            case atomicEthType of
                AtomicEthTypeBool -> compare x1 x2
                AtomicEthTypeInteger _ _ -> compare x1 x2
                AtomicEthTypeBytes _ -> compare x1 x2
                AtomicEthTypeString ->  compare x1 x2
    EthTypeArray elemType _ ->
        \(EthValueArray xs1) (EthValueArray xs2) ->
            compareEthValueArray tc elemType xs1 xs2
    EthTypeStruct sname ->
        withStructDef tc sname $ \memDefs ->
            \(EthValueStruct vs1) (EthValueStruct vs2) ->
                compareEthValueStruct tc memDefs vs1 vs2

compareEthValueArray ::
    TypeContext ctx ->
    EthType rep ->
    [EthValue ctx rep] ->
    [EthValue ctx rep] ->
    Ordering
compareEthValueArray _ _ [] [] =
    EQ
compareEthValueArray _ _ (_ : _) [] =
    GT
compareEthValueArray _ _ [] (_ : _) =
    LT
compareEthValueArray tc elemType (x1 : xs1) (x2 : xs2) =
    compareEthValue tc elemType x1 x2 <>
    compareEthValueArray tc elemType xs1 xs2

compareEthValueStruct ::
    TypeContext ctx ->
    HList MemberDef reps ->
    HList (EthValue ctx) reps ->
    HList (EthValue ctx) reps ->
    Ordering
compareEthValueStruct tc memDefs vs1 vs2 =
    hfoldMap
        (\(Pair (MemberDef mtype _) (Pair v1 v2)) ->
            compareEthValue tc mtype v1 v2
        )
        (hzip memDefs (hzip vs1 vs2))



type StructDef = HList MemberDef

{-| Structs are defines by their members, and each member is defined by its name and type.

    The member type's representation marker is exposed as the marker of the member itself. When collected into an
`HList`, its combined marker gives the representation of the entire structure, as a list of representations of
each of its members. -}
data MemberDef rep where
    MemberDef ::
        EthType rep ->
        Text.Text ->
        MemberDef rep

deriving instance Show (MemberDef rep)

instance TestEquality MemberDef where
    testEquality (MemberDef type1 name1) (MemberDef type2 name2)
        | Just Refl <- testEquality type1 type2
        , name1 == name2 =
            Just Refl
    testEquality _ _ =
        Nothing



{-| A type context is an `HMap` from struct names to their definitions (member lists).

    In the context of Haskell's type system, a `TypeContext` is a singleton — for each instance, the marker @ctx@
will take on a unique type of kind @`TMap` `SName` `StructDef`@. Combined with `EthType`'s markers, this makes
this representation of Solidity objects in Haskell almost an instance of dependent typing — where the values
of the `TypeContext` and `EthType` determine the exact type of an `EthValue`.

    With the marker "hidden" under an existential, @Some TypeContext@ practically becomes a normal map. To that end,
it posesses an instance of @Monoid (Some TypeContext)@, which allows one to construct a type context from individual
definitions. -}
newtype TypeContext ctx = TypeContext
    { typeContextMap :: HMap SName StructDef ctx
    }
    deriving newtype TestEquality

instance Show (TypeContext ctx) where
    showsPrec _ tc =
        showString "<TypeContext: " .
        showsTypeContext tc .
        showString ">"

{-| If both t`TypeContext`s have definitions for the same struct name, `(Data.Semigroup.<>)` will check that they
are equal, and throw an error if they are not. -}
instance Semigroup (Some TypeContext) where
    Some tc1 <> Some tc2 =
        usingSome (Some . TypeContext) $
            unionHMapWithKey
                (\sname sdef1 sdef2 ->
                    if sdef1 == sdef2
                        then sdef1
                        else error $ "Conflicting struct definitions for" <> show (usingSome fromSName sname)
                )
                (Some $ typeContextMap tc1)
                (Some $ typeContextMap tc2)

instance Monoid (Some TypeContext) where
    mempty = emptyTypeContext

{-| Produce an empty type context. -}
emptyTypeContext ::
    Some TypeContext
emptyTypeContext =
    usingSome (Some . TypeContext) emptyHMap

{-| Produce a type context with a single definition. -}
singletonTypeContext ::
    SName typename ->
    StructDef reps ->
    Some TypeContext
singletonTypeContext k v =
    usingSome (Some . TypeContext) (singletonHMap k v)

{-| Perform a safe lookup in a type context. If the given key is absent, `Nothing` is returned. -}
lookupStructDef ::
    TypeContext ctx ->
    SName typename ->
    Maybe (StructDef (Lookup typename ctx))
lookupStructDef tc sname =
    lookupHMap sname (typeContextMap tc)

{-| Perform a strict lookup in a type context. If the given key is absent, an `error` is raised.

    The continuation-passing style of this function allows one to put an arbitrary expression dependent on the check,
so that even if the struct definition itself ends up being unused due to lazyness, the key-presence check will be
performed anyway. -}
withStructDef ::
    TypeContext ctx ->
    SName typename ->
    (StructDef (Lookup typename ctx) -> r) ->
    r
withStructDef tc sname fn =
    case lookupStructDef tc sname of
        Nothing -> error "invalid type context"
        Just memDefs -> fn memDefs



{-| Prints a Solidity object into a `ShowS`-string. This is intended solely for debugging purposes, so the output
is not guaranteed to conform to any format or even be unambiguous.

    If you want to properly serialize a Solidity object, you should use `Data.SignTyped.Json.ethValueToJSON` or
`Data.SignTyped.Json.ethValueToEncoding` instead. -}
showsEthValue ::
    TypeContext ctx ->
    EthType rep ->
    EthValue ctx rep ->
    ShowS
showsEthValue _ (EthTypeAtomic atomType) (EthValueAtomic value) =
    case atomType of
        AtomicEthTypeBool ->
            if value
                then showString "true"
                else showString "false"
        AtomicEthTypeInteger _ _ ->
            shows value
        AtomicEthTypeBytes AtomicBytesSubtypeAddress ->
            shows (encodeChecksumAddress value)
        AtomicEthTypeBytes (AtomicBytesSubtypeShortBytes _) ->
            shows (encodeHexByteString value)
        AtomicEthTypeBytes AtomicBytesSubtypeLongBytes ->
            shows (encodeHexByteString value)
        AtomicEthTypeString ->
            shows (encodeSafeTextString value)
showsEthValue tc (EthTypeArray elemType _) (EthValueArray list) =
    showString "[" .
    showsEthValueArrayContents True tc elemType list .
    showString "]"
showsEthValue tc (EthTypeStruct sname) (EthValueStruct memVals) =
    showsEthValueStruct tc sname memVals

showsEthValueArrayContents ::
    Bool ->
    TypeContext ctx ->
    EthType rep ->
    [EthValue ctx rep] ->
    ShowS
showsEthValueArrayContents isFirst tc mtype (x : rest) =
    (if isFirst then id else showString ", ") .
    showsEthValue tc mtype x .
    showsEthValueArrayContents False tc mtype rest
showsEthValueArrayContents _ _ _ [] =
    id

showsEthValueStructMembers ::
    Bool ->
    TypeContext ctx ->
    StructDef reps ->
    HList (EthValue ctx) reps ->
    ShowS
showsEthValueStructMembers isFirst tc (MemberDef mtype mname :/ otherDefs) (mval :/ otherVals) =
    (if isFirst then id else showString ", ") .
    showString (Text.unpack mname) .
    showString " = " .
    showsEthValue tc mtype mval .
    showsEthValueStructMembers False tc otherDefs otherVals
showsEthValueStructMembers _ _ HEnd HEnd =
    id

showsEthValueStruct ::
    TypeContext ctx ->
    SName typename ->
    HList (EthValue ctx) (Lookup typename ctx) ->
    ShowS
showsEthValueStruct tc sname memVals =
    withStructDef tc sname $ \memDefs ->
        showString (Text.unpack $ fromSName sname) .
        showString " {" .
        showsEthValueStructMembers True tc memDefs memVals .
        showString "}"



{-| Prints a struct definition into a `ShowS`-string. This is intended solely for debugging purposes, so the output
is not guaranteed to conform to any format or even be unambiguous.

    If you want to properly serialize a struct definition, you should instead use
`Data.SignTyped.Json.toUnmarkedStructDef`, followed by any of the Aeson's serialization routines. -}
showsEthStructDef ::
    SName typename ->
    StructDef reps ->
    ShowS
showsEthStructDef sname memDefs =
    showString (Text.unpack $ fromSName sname) .
    showString "(" .
    showsEthStructDefMembers True memDefs .
    showString ")"

showsEthStructDefMembers ::
    Bool ->
    StructDef reps ->
    ShowS
showsEthStructDefMembers isFirst (MemberDef mtype mname :/ otherDefs) =
    (if isFirst then id else showString ",") .
    showString (Text.unpack (ethTypeName mtype)) .
    showString " " .
    showString (Text.unpack mname) .
    showsEthStructDefMembers False otherDefs
showsEthStructDefMembers _ HEnd =
    id

{-| Prints a type context into a `ShowS`-string. This is intended solely for debugging purposes, so the output
is not guaranteed to conform to any format or even be unambiguous.

    If you want to properly serialize a type context, you should instead use
`Data.SignTyped.Json.toUnmarkedTypeContext`, followed by any of the Aeson's serialization routines. -}
showsTypeContext ::
    TypeContext ctx ->
    ShowS
showsTypeContext tc =
    case
        foldrHMapWithKey
            (\sname memDefs (isLast, strs) ->
                (   False
                ,   showsEthStructDef sname memDefs .
                    (if isLast then id else showString "; ") .
                    strs
                )
            )
            (True, id)
            (typeContextMap tc)
      of
        (False, str) -> str
        (True, _) -> showString "empty"



{-| A Solidity object wrapped together with its type.

    This wrapper is intended to be used in test suites, where assertions expect the objects to have
standard `Show` and `Eq` instances, for example as arguments to `Test.Hspec.shouldBe`. -}
data SomeEthValue
    = forall ctx repr.
    SomeEthValue
        (TypeContext ctx)
        (EthType repr)
        (EthValue ctx repr)

instance Show SomeEthValue where
    showsPrec _ (SomeEthValue tc ethType ethValue) =
        showString "<SomeEthValue: " .
        showsEthValue tc ethType ethValue .
        showString ">"

instance Eq SomeEthValue where
    SomeEthValue tc1 ethType1 ethValue1 == SomeEthValue tc2 ethType2 ethValue2
        | Just Refl <- testEquality tc1 tc2
        , Just Refl <- testEquality ethType1 ethType2 =
            eqEthValue tc1 ethType1 ethValue1 ethValue2
        | otherwise =
            False



{-| Performs validation checks on the given object and returns the list of errors.

    This function checks for additional conditions imposed by the Solidity type definition that are not part
of the representation's Haskell type itself, such as the integer values being in range of their types, or fixed-size
arrays having a proper number of elements.

    The default handles from `Data.SignTyped.Class.SignableValue`, as well as
`Data.SignTyped.Json.ethValueParseJSON` perform their own validation to produce only correct Solidity objects,
so this specific function is better suited as a debug tool rather than a routine checker in the main code. -}
validateEthValue ::
    TypeContext ctx ->
    EthType rep ->
    EthValue ctx rep ->
    [String]
validateEthValue tc ethType ethValue =
    validateEthValueWorker (showString "$") tc ethType ethValue []

{-| Internal recursive function for `validateEthValue`.

    The first argument is a `ShowS`-encoded prefix of the value's location (e.g. @showString ".elems[3].first"@).

    The partial list is returned as a difference-list.

    The default entrypoint is defined like this:

    @
        validateEthValue tc ethType ethValue =
            validateEthValueWorker (showString "$") tc ethType ethValue []
    @ -}
validateEthValueWorker ::
    ShowS ->
    TypeContext ctx ->
    EthType rep ->
    EthValue ctx rep ->
    [String] ->
    [String]
validateEthValueWorker scope _ (EthTypeAtomic atomType) (EthValueAtomic value) =
    case atomType of
        AtomicEthTypeBool ->
            id
        AtomicEthTypeInteger signedness bitSize
            | isValidInteger signedness bitSize value ->
                id
            | otherwise ->
                let typename =
                        (matchAtomicIntegerSignedness "uint" "int" signedness) <>
                        show bitSize
                in
                (:) (scope $ ": Integer value " <> show value <> " is outside the range of \"" <> typename <> "\"")
        AtomicEthTypeBytes AtomicBytesSubtypeAddress
            | BS.length value == 20 ->
                id
            | otherwise ->
                (:) (scope $
                        ": Byte value " <> show (encodeHexByteString value) <>
                        " is not a valid \"address\""
                    )
        AtomicEthTypeBytes (AtomicBytesSubtypeShortBytes size)
            | BS.length value == size ->
                id
            | otherwise ->
                (:) (scope $
                        ": Byte value " <> show (encodeHexByteString value) <>
                        " is not a valid \"bytes" <> show size <> "\""
                    )
        AtomicEthTypeBytes AtomicBytesSubtypeLongBytes ->
            id
        AtomicEthTypeString ->
            id
validateEthValueWorker scope tc arrayType@(EthTypeArray elemType mbSize) (EthValueArray list) =
    (case mbSize of
        Nothing ->
            id
        Just size
            | length list /= size ->
                (:) (scope $ ": Invalid array size for " <> show (ethTypeName arrayType))
            | otherwise ->
                id
    ) .
    (\after ->
        foldr
            (\(i, x) ->
                validateEthValueWorker (scope . showsSubscript i) tc elemType x
            )
            after
            (zip [0 :: Int ..] list)
    )
  where
    showsSubscript i =
        showString "[" . shows i . showString "]"
validateEthValueWorker scope tc (EthTypeStruct sname) (EthValueStruct memVals) =
    withStructDef tc sname $ \memDefs ->
        \after ->
            hfoldr
                (\(Pair (MemberDef mtype mname) mval) ->
                    validateEthValueWorker (scope . showsAccess mname) tc mtype mval
                )
                after
                (hzip memDefs memVals)
  where
    showsAccess name =
        showString ("." <> Text.unpack name)

{-| Check that the given integer fits within the given type. -}
isValidInteger :: AtomicIntegerSignedness -> AtomicIntegerBitSize -> Integer -> Bool
isValidInteger AtomicIntegerUnsigned bitSize x =
    0 <= x && x < 2^bitSize
isValidInteger AtomicIntegerSigned bitSize x =
    (-bv) <= x && x < bv
  where
    bv = 2^(bitSize-1)
