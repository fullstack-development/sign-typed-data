{-# OPTIONS_GHC -Wno-identities #-}
{-# LANGUAGE TemplateHaskell #-}

{-| This modules defines the JSON format of Solidity types and objects.

    For types, we define an intermediate representation, composed of conventional @data@s with standard Aeson
instances, and functions for conversion between this intermediate representation and "Data.SignTypes.Structure"'s
objects.

    For values, we convert between `EthValue`'s and JSON directly. -}
module Data.SignTyped.Json
    {-* Unmarked type context representation -}
    ( UnmarkedTypeContext
    , UnmarkedStructDef
    , UnmarkedMemberDef (..)
    , UnmarkedEthType

    {-* Serializing into the unmarked representation -}
    , toUnmarkedTypeContext
    , toUnmarkedStructDef
    , toUnmarkedMemberDef
    , toUnmarkedEthType

    {-* Parsing from the unmarked representation -}
    , parseUnmarkedTypeContext
    , parseUnmarkedStructDef
    , parseUnmarkedMemberDef
    , parseUnmarkedEthType

    {-* Serializing and parsing Solidity values -}
    , ethValueToJSON
    , ethValueToEncoding
    , ethValueParseJSON

    {-* Auxiliary functions -}
    , isJsonRepresentableInteger
    )
where

import Control.Applicative
import Control.Monad
import Data.Foldable
import Data.Monoid
import Data.SignTyped.Type.HList
import Data.SignTyped.Type.HMap
import Data.SignTyped.Type.SName
import Data.SignTyped.Type.Some
import Data.SignTyped.Structure
import Data.SignTyped.Util
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding as Aeson.Encoding
import qualified Data.Aeson.Key as Aeson.Key
import qualified Data.Aeson.KeyMap as Aeson.KeyMap
import qualified Data.Aeson.TH as Aeson.TH
import qualified Data.Aeson.Types as Aeson.Types
import qualified Data.Attoparsec.Text as Atp
import qualified Data.ByteString as BS
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Scientific as Scientific
import qualified Data.Text as Text
import qualified Data.Vector as V



{-| A representation of a type context without the type-level markings, used as an intermediate between
the marked `TypeContext` and completely untyped `Aeson.Value`. -}
type UnmarkedTypeContext = Map.Map Text.Text UnmarkedStructDef

type UnmarkedStructDef = [UnmarkedMemberDef]

data UnmarkedMemberDef = UnmarkedMemberDef
    { _type :: UnmarkedEthType
    , _name :: Text.Text
    }
    deriving (Show, Eq, Ord)

type UnmarkedEthType = Text.Text

Aeson.TH.deriveJSON
    (Aeson.defaultOptions
        { Aeson.fieldLabelModifier = drop 1
        }
    )
    'UnmarkedMemberDef



toUnmarkedTypeContext :: TypeContext ctx -> UnmarkedTypeContext
toUnmarkedTypeContext tc =
    foldMapHMapWithKey
        (\sname memDefs ->
            Map.singleton
                (fromSName sname)
                (toUnmarkedStructDef memDefs)
        )
        (typeContextMap tc)

toUnmarkedStructDef :: StructDef reps -> UnmarkedStructDef
toUnmarkedStructDef memDefs =
    htoList $ hmap (Const . toUnmarkedMemberDef) memDefs

toUnmarkedMemberDef :: MemberDef rep -> UnmarkedMemberDef
toUnmarkedMemberDef (MemberDef mtype mname) =
    UnmarkedMemberDef (toUnmarkedEthType mtype) mname

toUnmarkedEthType :: EthType rep -> UnmarkedEthType
toUnmarkedEthType ethType =
    ethTypeName ethType



{-| Convert an unmarked type context into a proper one.

    This function also verifies that all types that are used in the context are either defined in the same context,
or are built-in Solidity types. -}
parseUnmarkedTypeContext :: UnmarkedTypeContext -> Aeson.Types.Parser (Some TypeContext)
parseUnmarkedTypeContext unmarkedTypeContext = do
    (dependencySet, definitionSet, someTypeContext) <-
        getAp $
        Map.foldMapWithKey
            (\structName jsonStructDef ->
                Ap $ do
                    (structDeps, someStructDef) <-
                        parseUnmarkedStructDef jsonStructDef
                            Aeson.<?> Aeson.Types.Key (Aeson.Key.fromText structName)
                    let tc1 =
                            withSomeSName structName $ \sname ->
                            withSome someStructDef $ \mdefs ->
                                singletonTypeContext sname mdefs
                    pure (structDeps, Set.singleton structName, tc1)
            )
            unmarkedTypeContext
    let undefinedNames = Set.difference dependencySet definitionSet
    if Set.null undefinedNames
        then pure someTypeContext
        else fail $
            "Type names referenced but not defined: " <>
            (List.intercalate ", " $ map show $ Set.toList undefinedNames)

{-| Parse an unmarked struct definition.

    This function also verifies that the there are no members with the same name.

    In addition to the proper struct definition, this function also returns the set of custom types that are
used in this struct. -}
parseUnmarkedStructDef :: UnmarkedStructDef -> Aeson.Types.Parser (Set.Set Text.Text, Some StructDef)
parseUnmarkedStructDef unmarkedMemberDefs = do
    checkDuplicates Set.empty unmarkedMemberDefs
    (dependencySetList, memberList) <-
        fmap unzip $ forM (zip [0..] unmarkedMemberDefs) $ \(i, mdef) ->
            parseUnmarkedMemberDef mdef
                Aeson.<?> Aeson.Types.Index i
    pure (Set.unions dependencySetList, hsequenceSome memberList)
  where
    checkDuplicates _ [] =
        pure ()
    checkDuplicates prevNames (UnmarkedMemberDef _ name : rest) = do
        if Set.member name prevNames
            then fail $ "Member name appears more than once: " <> show name
            else checkDuplicates (Set.insert name prevNames) rest

{-| Parse an unmarked struct member definition.

    In addition to the proper member definition, this function also returns the set of custom types that are
used in the member. -}
parseUnmarkedMemberDef :: UnmarkedMemberDef -> Aeson.Types.Parser (Set.Set Text.Text, Some MemberDef)
parseUnmarkedMemberDef (UnmarkedMemberDef typeName memberName) = do
    (dependencySet, Some ethType) <- parseUnmarkedEthType typeName
    pure (dependencySet, Some $ MemberDef ethType memberName)

{-| Parse an unmarked type definition.

    In addition to the proper type definition, this function also returns the set of custom types that are
used here. -}
parseUnmarkedEthType :: Text.Text -> Aeson.Types.Parser (Set.Set Text.Text, Some EthType)
parseUnmarkedEthType bn =
    case Atp.parseOnly typeParser bn of
        Left _ -> fail $ "Invalid type: " <> show bn
        Right x -> pure x
  where
    typeParser = do
        baseTypeName <- Atp.takeWhile (/= '[')
        (dependencySet, baseType) <-
            case Map.lookup baseTypeName atomicTypes of
                Just (Some atype) ->
                    pure (Set.empty, Some $ EthTypeAtomic atype)
                Nothing ->
                    pure (Set.singleton baseTypeName, withSomeSName baseTypeName (Some . EthTypeStruct))
        subscripts <- many subscriptParser
        Atp.endOfInput
        let ethType =
                foldl'
                    (\(Some t) mbSize ->
                        Some $ EthTypeArray t mbSize
                    )
                    baseType
                    subscripts
        pure (dependencySet, ethType)
    subscriptParser = do
        _ <- Atp.char '['
        Atp.skipSpace
        ds <- Atp.takeWhile (\c -> '0' <= c && c <= '9')
        mbSize <-
            if Text.null ds
                then do
                    pure Nothing
                else do
                    Atp.skipSpace
                    pure $ Just $ toInt ds
        _ <- Atp.char ']'
        Atp.skipSpace
        pure mbSize
    toInt = Text.foldl' (\i w -> i * 10 + fromEnum w - 48) (0 :: Int)



{-| Implementation of `Aeson.toJSON` for a typed Solidity object. -}
ethValueToJSON ::
    TypeContext ctx ->
    EthType rep ->
    EthValue ctx rep ->
    Aeson.Value
ethValueToJSON _ (EthTypeAtomic atomType) (EthValueAtomic value) =
    case atomType of
        AtomicEthTypeBool ->
            Aeson.toJSON value
        AtomicEthTypeInteger _ _ ->
            if isJsonRepresentableInteger value
                then Aeson.toJSON value
                else Aeson.toJSON $ encodeHexInteger value
        AtomicEthTypeBytes AtomicBytesSubtypeAddress ->
            Aeson.toJSON $ encodeChecksumAddress value
        AtomicEthTypeBytes (AtomicBytesSubtypeShortBytes _) ->
            Aeson.toJSON $ encodeHexByteString value
        AtomicEthTypeBytes AtomicBytesSubtypeLongBytes ->
            Aeson.toJSON $ encodeHexByteString value
        AtomicEthTypeString ->
            Aeson.toJSON $ encodeSafeTextString value
ethValueToJSON tc (EthTypeArray elemType _) (EthValueArray list) =
    Aeson.toJSON $ map (ethValueToJSON tc elemType) list
ethValueToJSON tc (EthTypeStruct sname) (EthValueStruct memVals) =
    withStructDef tc sname $ \memDefs ->
        Aeson.object $
            hfoldMap
                (\(Pair (MemberDef mtype mname) mval) ->
                    [(Aeson.Key.fromText mname, ethValueToJSON tc mtype mval)]
                )
                (hzip memDefs memVals)

{-| Implementation of `Aeson.toEncoding` for a typed Solidity object. -}
ethValueToEncoding ::
    TypeContext ctx ->
    EthType rep ->
    EthValue ctx rep ->
    Aeson.Encoding
ethValueToEncoding _ (EthTypeAtomic atomType) (EthValueAtomic value) =
    case atomType of
        AtomicEthTypeBool ->
            Aeson.toEncoding value
        AtomicEthTypeInteger _ _ ->
            if isJsonRepresentableInteger value
                then Aeson.toEncoding value
                else Aeson.toEncoding $ encodeHexInteger value
        AtomicEthTypeBytes AtomicBytesSubtypeAddress ->
            Aeson.toEncoding $ encodeChecksumAddress value
        AtomicEthTypeBytes (AtomicBytesSubtypeShortBytes _) ->
            Aeson.toEncoding $ encodeHexByteString value
        AtomicEthTypeBytes AtomicBytesSubtypeLongBytes ->
            Aeson.toEncoding $ encodeHexByteString value
        AtomicEthTypeString ->
            Aeson.toEncoding $ encodeSafeTextString value
ethValueToEncoding tc (EthTypeArray elemType _) (EthValueArray list) =
    Aeson.Encoding.list (ethValueToEncoding tc elemType) list
ethValueToEncoding tc (EthTypeStruct sname) (EthValueStruct memVals) =
    withStructDef tc sname $ \memDefs ->
        Aeson.pairs $
            hfoldMap
                (\(Pair (MemberDef mtype mname) mval) ->
                    Aeson.Encoding.pair
                        (Aeson.Key.fromText mname)
                        (ethValueToEncoding tc mtype mval)
                )
                (hzip memDefs memVals)

{-| Implementation of `Aeson.parseJSON` for a typed Solidity object.

    @null@ is accepted, and corresponds to `defaultEthValue`. -}
ethValueParseJSON ::
    TypeContext ctx ->
    EthType rep ->
    Aeson.Value ->
    Aeson.Types.Parser (EthValue ctx rep)
ethValueParseJSON tc ethType jsonValue =
    case (ethType, jsonValue) of
        (_, Aeson.Null) -> do
            pure $ defaultEthValue tc ethType
        (EthTypeAtomic AtomicEthTypeBool, Aeson.Bool b) -> do
            pure $ EthValueAtomic b
        (EthTypeAtomic (AtomicEthTypeInteger signedness bitSize), Aeson.Number s) -> do
            case Scientific.toBoundedInteger s of
                Just (AnyEthInteger n)
                    | isValidInteger signedness bitSize n ->
                        pure $ EthValueAtomic n
                _ ->
                    fail "Invalid number"
        (EthTypeAtomic (AtomicEthTypeInteger signedness bitSize), Aeson.String t) -> do
            case readNumber t of
                Just n
                    | isValidInteger signedness bitSize n ->
                        pure $ EthValueAtomic n
                _ ->
                    fail "Invalid number"
        (EthTypeAtomic (AtomicEthTypeBytes AtomicBytesSubtypeAddress), Aeson.String t) -> do
            case readChecksumAddress t of
                Just bs -> pure $ EthValueAtomic bs
                Nothing -> fail "Invalid address"
        (EthTypeAtomic (AtomicEthTypeBytes (AtomicBytesSubtypeShortBytes n)), Aeson.String t) -> do
            case readHexBytes t of
                Just bs ->
                    if BS.length bs == n
                        then pure $ EthValueAtomic bs
                        else fail "Invalid byte count"
                Nothing -> fail "Invalid bytes value"
        (EthTypeAtomic (AtomicEthTypeBytes AtomicBytesSubtypeLongBytes), Aeson.String t) -> do
            case readHexBytes t of
                Just bs -> pure $ EthValueAtomic bs
                Nothing -> fail "Invalid bytes value"
        (EthTypeAtomic AtomicEthTypeString, Aeson.String t) -> do
            case readSafeTextString t of
                Just bs -> pure $ EthValueAtomic bs
                Nothing -> fail "Invalid string value"
        (EthTypeArray atomType mbSize, Aeson.Array xs) -> do
            case mbSize of
                Nothing ->
                    pure ()
                Just size ->
                    when (V.length xs /= size) $ do
                        fail "Invalid array size"
            fmap EthValueArray $ forM [0 .. V.length xs - 1] $ \i ->
                ethValueParseJSON tc atomType (xs V.! i)
                    Aeson.<?> Aeson.Types.Index i
        (EthTypeStruct sname, Aeson.Object xs) -> do
            case lookupStructDef tc sname of
                Nothing ->
                    fail $ "Unknown type: " <> show (fromSName sname)
                Just memDefs ->
                    fmap EthValueStruct $
                    htraverse
                        (\(MemberDef mtype mname) -> do
                            let mkey = Aeson.Key.fromText mname
                            let value = maybe Aeson.Null id $ Aeson.KeyMap.lookup mkey xs
                            ethValueParseJSON tc mtype value
                                Aeson.<?> Aeson.Types.Key mkey
                        )
                        memDefs
        _ ->
            fail "Invalid type"

newtype AnyEthInteger = AnyEthInteger Integer
    deriving newtype (Eq, Ord, Enum, Num, Real, Integral)

instance Bounded AnyEthInteger where
    minBound = - 2^(255::Int)
    maxBound = 2^(256::Int) - 1



{-| IEEE double-presicion numbers can represent integers within the range [-2^52 .. 2^52] exactly.

    This function tests whether the given integer belongs to this range. -}
isJsonRepresentableInteger :: Integer -> Bool
isJsonRepresentableInteger i =
    (- 2^(52::Int)) <= i && i <= 2^(52::Int)



atomicTypes :: Map.Map Text.Text (Some AtomicEthType)
atomicTypes =
    Map.singleton "bool" (Some AtomicEthTypeBool)
  <>
    Map.singleton "address" (Some $ AtomicEthTypeBytes AtomicBytesSubtypeAddress)
  <>
    Map.singleton "bytes" (Some $ AtomicEthTypeBytes AtomicBytesSubtypeLongBytes)
  <>
    Map.singleton "string" (Some AtomicEthTypeString)
  <>
    foldMap
        (\bn ->
            Map.singleton
                (Text.pack $ "int" <> show bn)
                (Some $ AtomicEthTypeInteger AtomicIntegerSigned bn)
          <>
            Map.singleton
                (Text.pack $ "uint" <> show bn)
                (Some $ AtomicEthTypeInteger AtomicIntegerUnsigned bn)
        )
        [8, 16 .. 256 :: Int]
  <>
    foldMap
        (\bn ->
            Map.singleton
                (Text.pack $ "bytes" <> show bn)
                (Some $ AtomicEthTypeBytes $ AtomicBytesSubtypeShortBytes bn)
        )
        [1 .. 32 :: Int]
