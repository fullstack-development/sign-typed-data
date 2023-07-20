module Data.SignTyped.Type.Some
    ( Some (Some)
    , usingSome
    , withSome
    )
where

import Data.Kind
import Data.SignTyped.Type.TestOrdering
import Data.Type.Equality
import qualified GHC.Exts
import qualified Unsafe.Coerce

{-| A wrapper for existential quantification.

    Semantically, it's definition is:

    @
        data Some (f :: k -> Type) = forall (a :: k). Some (f a)
    @

    It's primarily intended to be used with marker GADTs and singleton types, such as
`Data.SignTyped.Type.SName.SName`. -}
type Some :: (k -> Type) -> Type
newtype Some f = UnsafeSome GHC.Exts.Any

pattern Some ::
    forall f. () =>
    forall t. () =>
    f t ->
    Some f
pattern Some x <- (Unsafe.Coerce.unsafeCoerce -> x)
  where
    Some x = UnsafeSome (Unsafe.Coerce.unsafeCoerce x)

{-# COMPLETE Some #-}

instance (forall t. Show (f t)) => Show (Some f) where
    showsPrec d (Some x) =
        showParen (d > 10) $
            showString "Some " .
            showsPrec 11 x

instance TestEquality f => Eq (Some f) where
    Some x == Some y =
        maybe False (const True) (testEquality x y)

instance TestOrdering f => Ord (Some f) where
    compare (Some x) (Some y) =
        case testOrdering x y of
            LTI -> LT
            EQI -> EQ
            GTI -> GT

usingSome :: (forall t. f t -> r) -> Some f -> r
usingSome fn (Some x) = fn x

withSome :: Some f -> (forall t. f t -> r) -> r
withSome (Some x) fn = fn x
