{-# OPTIONS_GHC -Wno-redundant-constraints #-}

{-| This module defines a singleton heterogeneous map.

    This map provides a mapping that exists on both type-level and value-level. The keys of the map are singletons,
where each distinct value has its own type marker. The values are arbitrary GADTs that use the type parameter as a
marker. For each mapping from a key object and a value object, there is a corresponding mapping from the key's
to the value's type markers inside the map's state token.

    The state token itself is not constructive, it has no direct representation in the Haskell's type system.
Instead, it is being kept as an abstract existential, which is consistently the same for the same object, but
distinct for any two non-equivalent objects. The presence of a particular mapping inside the state token is asserted
axiomatically by the `HMap`-operating functions.

    Whenever a new `HMap` object is created, it is wrapped in a `Some`. This effectively makes GHC spawn a new
existential type for each such creation. -}
module Data.SignTyped.Type.HMap
    ( TMap
    , Lookup
    , HMap
    , emptyHMap
    , singletonHMap
    , unionHMapWithKey
    , lookupHMap
    , foldMapHMapWithKey
    , foldrHMapWithKey
    , Some (..)
    )
where

import Data.Kind
import Data.SignTyped.Type.Some
import Data.SignTyped.Type.TestOrdering
import Data.Type.Equality
import qualified Data.Map.Lazy as Map
import qualified Unsafe.Coerce

{-| A kind for state tokens of `HMap`s. -}
data TMap kk kv

{-| A type family that represents a lookup result within a specific state token.

    There is no constructive definition for this type family, its instances are produced axiomatically by `HMap`'s
functions. -}
type Lookup :: kk -> TMap kk kv -> kv
type family Lookup k ms where {}

{-| A singleton heterogeneous map.

    The last parameter is a type-level token corresponding to the map's value. -}
type HMap :: (kk -> Type) -> (kv -> Type) -> TMap kk kv -> Type
newtype HMap kf vf s = UnsafeHMap (Map.Map (Some kf) (Some vf))

instance (forall kt. Show (kf kt), forall vt. Show (vf vt)) => Show (HMap kf vf s) where
    showsPrec d (UnsafeHMap m)
        | Map.null m =
            showString "emptyHMap"
        | otherwise =
            showParen (d > 6) $
                foldr (.) id elemStrings
      where
        elemStrings :: [ShowS]
        elemStrings =
            mapExceptLast (. showString " <> ") $
            map (\(Some k, Some v) ->
                showString "singletonHMap " .
                showsPrec 11 k .
                showString " " .
                showsPrec 11 v
            ) $
            Map.toList m
        mapExceptLast :: (a -> a) -> [a] -> [a]
        mapExceptLast fn (a : rest@(_ : _)) =
            fn a : mapExceptLast fn rest
        mapExceptLast _ lastOrNull =
            lastOrNull

instance (TestEquality kf, TestEquality vf) => TestEquality (HMap kf vf) where
    testEquality (UnsafeHMap m1 :: HMap kf vf s1) (UnsafeHMap m2 :: HMap kf vf s2) =
        if m1 == m2
            then
                case Unsafe.Coerce.unsafeEqualityProof @s1 @s2 of
                    Unsafe.Coerce.UnsafeRefl ->
                        Just Refl
            else
                Nothing

emptyHMap :: Some (HMap kf vf)
emptyHMap = Some $ UnsafeHMap Map.empty

singletonHMap :: (SingletonConstructor kf) => kf kt -> vf vt -> Some (HMap kf vf)
singletonHMap k v = Some $ UnsafeHMap $ Map.singleton (Some k) (Some v)

unionHMapWithKey ::
    (SingletonConstructor kf) =>
    (Some kf -> Some vf -> Some vf -> Some vf) ->
    Some (HMap kf vf) ->
    Some (HMap kf vf) ->
    Some (HMap kf vf)
unionHMapWithKey fn (Some (UnsafeHMap m1)) (Some (UnsafeHMap m2)) =
    Some $ UnsafeHMap $
        Map.unionWithKey
            fn
            m1
            m2

{-| If the same key is present in both `HMap`s, `(Data.Semigroup.<>)` will throw an error. -}
instance (SingletonConstructor kf, forall kt. Show (kf kt)) => Semigroup (Some (HMap kf vf)) where
    (<>) = unionHMapWithKey (\(Some name) _ _ -> error $ "duplicate key: " ++ show name)

instance (SingletonConstructor kf, forall kt. Show (kf kt)) => Monoid (Some (HMap kf vf)) where
    mempty = emptyHMap

lookupHMap ::
    forall kf kt vf s.
    (SingletonConstructor kf) =>
    kf kt ->
    HMap kf vf s ->
    Maybe (vf (Lookup kt s))
lookupHMap k (UnsafeHMap hm) =
    case Map.lookup (Some k) hm of
        Nothing -> Nothing
        Just (Some (v :: vf vt)) ->
            case Unsafe.Coerce.unsafeEqualityProof @(Lookup kt s) @vt of
                Unsafe.Coerce.UnsafeRefl ->
                    Just v

foldMapHMapWithKey ::
    Monoid m =>
    (forall kt. kf kt -> vf (Lookup kt s) -> m) ->
    HMap kf vf s ->
    m
foldMapHMapWithKey fn (UnsafeHMap nm) =
    Map.foldMapWithKey
        (Unsafe.Coerce.unsafeCoerce fn)
        nm

foldrHMapWithKey ::
    (forall kt. kf kt -> vf (Lookup kt s) -> r -> r) ->
    r ->
    HMap kf vf s ->
    r
foldrHMapWithKey fn z (UnsafeHMap nm) =
    Map.foldrWithKey
        (Unsafe.Coerce.unsafeCoerce fn)
        z
        nm
