module Data.SignTyped.Hashing
    ( hashEthValue
    , hashEthStruct
    , structLocalContext
    , structTypeFullSignature
    )
where

import Control.Monad
import Control.Monad.ST
import Data.Foldable
import Data.Functor.Const
import Data.Maybe
import Data.STRef
import Data.SignTyped.Type.HList
import Data.SignTyped.Type.HMap
import Data.SignTyped.Type.SName
import Data.SignTyped.Structure
import Data.SignTyped.Util
import qualified Data.ByteString as BS
import qualified Data.Map.Lazy as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding

{- In this module, we temporarily store `error` values in maps, so we need them to be lazy. -}

{-| Produce the hash of the data to be signed.

    This corresponds to the @encodeData@ operation of the specification. -}
hashEthValue ::
    TypeContext ctx ->
    EthType rep ->
    EthValue ctx rep ->
    BS.ByteString
hashEthValue _ (EthTypeAtomic atomType) (EthValueAtomic value) =
    case atomType of
        AtomicEthTypeBool ->
            if value
                then integer256 1
                else integer256 0
        AtomicEthTypeInteger _ _ ->
            integer256 value
        AtomicEthTypeBytes AtomicBytesSubtypeAddress ->
            "\0\0\0\0\0\0\0\0\0\0\0\0" <> value
        AtomicEthTypeBytes (AtomicBytesSubtypeShortBytes _) ->
            value <> BS.replicate (32 - BS.length value) 0
        AtomicEthTypeBytes AtomicBytesSubtypeLongBytes ->
            keccak256 value
        AtomicEthTypeString ->
            keccak256 $ Text.Encoding.encodeUtf8 value
hashEthValue tc (EthTypeArray elemType _) (EthValueArray xs) =
    keccak256 $
        foldMap (hashEthValue tc elemType) xs
hashEthValue tc (EthTypeStruct sname) (EthValueStruct memVals) =
    hashEthStruct tc sname memVals

{-| Produce the hash of the struct value to be signed.

    This corresponds to the @hashStruct@ operation of the specification. -}
hashEthStruct ::
    TypeContext ctx ->
    SName typename ->
    HList (EthValue ctx) (Lookup typename ctx) ->
    BS.ByteString
hashEthStruct tc sname memVals =
    withStructDef tc sname $ \memDefs ->
        keccak256 $
            keccak256 (structTypeFullSignature tc sname)
          <>
            hfoldMap
                (\(Pair (MemberDef mtype _) mval) -> hashEthValue tc mtype mval)
                (hzip memDefs memVals)

{-| Collects all structs transitively used in defining the current one, including itself, and returns a map
from struct names to their signatures.

    For example, given this set of definitions in the type context:

    @
        struct Person {
            string name;
            address wallet;
        }

        struct Mail {
            Person from;
            Person to;
            string contents;
        }
    @

    This function will return these results:

    @
        structLocalContext tc (SName \@\"Person\") =
            fromList
                [(\"Person\", "Person(string name,address wallet)")]

        structLocalContext tc (SName \@\"Mail\") =
            fromList
                [(\"Mail\",   "Mail(Person from,Person to,string contents)"),
                 (\"Person\", "Person(string name,address wallet)")]
    @

    This function is used as part of `structTypeFullSignature`. -}
structLocalContext ::
    TypeContext ctx ->
    SName typename ->
    Map.Map Text.Text BS.ByteString
structLocalContext tc rootType =
    runST $ do
        cache <- newSTRef Map.empty
        goStruct cache rootType
        readSTRef cache
  where
    goStruct :: STRef s (Map.Map Text.Text BS.ByteString) -> SName t -> ST s ()
    goStruct cache t =
        withStructDef tc t $ \memDefs -> do
            cacheValue <- readSTRef cache
            case Map.lookup (fromSName t) cacheValue of
                Nothing -> do
                    modifySTRef' cache $
                        Map.insert
                            (fromSName t)
                            (error "pending")
                    mdefStrs <- goMembers cache memDefs
                    modifySTRef' cache $
                        Map.insert
                            (fromSName t)
                            (Text.Encoding.encodeUtf8 $ fromSName t <> "(" <> Text.intercalate "," mdefStrs <> ")")
                Just _ -> do
                    pure ()
    goMembers :: STRef s (Map.Map Text.Text BS.ByteString) -> StructDef reps -> ST s [Text.Text]
    goMembers cache memDefs =
        fmap (catMaybes . htoList) $
            htraverse
                (\(MemberDef mtype nameStr) -> do
                    typeStr <- goType cache mtype
                    pure $ Const $ Just $ typeStr <> " " <> nameStr
                )
                memDefs
    goType :: STRef s (Map.Map Text.Text BS.ByteString) -> EthType rep -> ST s Text.Text
    goType cache mtype = do
        case mtype of
            EthTypeStruct sname -> goStruct cache sname
            EthTypeArray subtype _ -> void $ goType cache subtype
            _ -> pure ()
        pure $ ethTypeName mtype

{-| Returns the argument of the type hash of the selected struct.

    This corresponds to the @encodeType@ operation in the specification. -}
structTypeFullSignature ::
    TypeContext ctx ->
    SName typename ->
    BS.ByteString
structTypeFullSignature tc rootType =
    let localContext = structLocalContext tc rootType
        !rootDef = localContext Map.! fromSName rootType
        strippedContext = Map.delete (fromSName rootType) localContext
    in
    rootDef <> fold strippedContext
