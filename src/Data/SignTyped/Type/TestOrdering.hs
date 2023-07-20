module Data.SignTyped.Type.TestOrdering
    ( TestOrdering (..)
    , OrderingI (..)
    , SingletonConstructor
    )
where

import Data.Kind
import Data.Type.Equality
import Data.Type.Ord
import Type.Reflection
import qualified Unsafe.Coerce

class TestEquality f => TestOrdering f where
    testOrdering :: f a -> f b -> OrderingI a b

{-| This typeclass declares that a type constructor produces singletons. That is, for each possible type argument @a@,
there is at most one possible object of type @f a@.

    In other words, the instances of this class must satisfy the law:

    prop> testOrdering (fa :: f a) (fb :: f b) == EQI    ===>    a !~ b

    Examples of good instances of this class are the singletons from "GHC.TypeLits", and also t`TypeRep` and
`Data.SignTyped.Type.SName.SName`.

    There are no additional methods to implement in this class, it is only used as a marker.-}
type SingletonConstructor :: (k -> Type) -> Constraint
class (TestOrdering f) => SingletonConstructor f

instance TestOrdering TypeRep where
    testOrdering (tr1 :: TypeRep a) (tr2 :: TypeRep b) =
        case compare (SomeTypeRep tr1) (SomeTypeRep tr2) of
            LT ->
                case Unsafe.Coerce.unsafeEqualityProof @(Compare a b) @'LT of
                    Unsafe.Coerce.UnsafeRefl -> LTI
            EQ ->
                case Unsafe.Coerce.unsafeEqualityProof @(Compare a b) @'EQ of
                    Unsafe.Coerce.UnsafeRefl ->
                        case Unsafe.Coerce.unsafeEqualityProof @a @b of
                            Unsafe.Coerce.UnsafeRefl -> EQI
            GT ->
                case Unsafe.Coerce.unsafeEqualityProof @(Compare a b) @'GT of
                    Unsafe.Coerce.UnsafeRefl -> GTI

instance SingletonConstructor TypeRep
