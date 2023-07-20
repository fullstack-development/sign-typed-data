{-# LANGUAGE ImpredicativeTypes #-}

{-| Singleton type for names that use `Text.Text` for representation, in the style of "GHC.TypeLits". -}
module Data.SignTyped.Type.SName
    ( SName (SName)
    , KnownName (..)
    , withSName
    , withSomeSName
    , fromSName
    , toSomeSName
    , Symbol
    )
where

import Data.Proxy
import Data.SignTyped.Type.Some
import Data.SignTyped.Type.TestOrdering
import Data.String
import Data.Type.Equality
import GHC.TypeLits
import qualified Data.Text as Text
import qualified Unsafe.Coerce

{-| Singleton for a specific name.

    To create a known t`SName` constant, use @v`SName` \@"str"@ or @`nameSing` \@"str"@.

    To create an t`SName` from a run-time value, use `withSomeSName`.

    To obtain the value inside, use `fromSName`. -}
newtype SName (sym :: Symbol) = UnsafeSName Text.Text

instance Show (SName sym) where
    showsPrec d (UnsafeSName s) =
        showParen (d > 10) $
            showString "SName @" .
            showsPrec 11 s

{-| A class for known names. Any known `Symbol` can be converted to a name. -}
class KnownName sym where
    {-| The singleton for the name. -}
    nameSing :: SName sym

instance (KnownSymbol sym) => KnownName sym where
    nameSing = UnsafeSName $ fromString $ symbolVal $ Proxy @sym

instance TestEquality SName where
    testEquality (UnsafeSName x) (UnsafeSName y)
        | x == y = Unsafe.Coerce.unsafeCoerce $ Just Refl
        | otherwise = Nothing

instance TestOrdering SName where
    testOrdering (UnsafeSName x :: SName a) (UnsafeSName y :: SName b) =
        case compare x y of
            LT ->
                case Unsafe.Coerce.unsafeEqualityProof @(CmpSymbol a b) @'LT of
                    Unsafe.Coerce.UnsafeRefl -> LTI
            EQ ->
                case Unsafe.Coerce.unsafeEqualityProof @a @b of
                    Unsafe.Coerce.UnsafeRefl -> EQI
            GT ->
                case Unsafe.Coerce.unsafeEqualityProof @(CmpSymbol a b) @'GT of
                    Unsafe.Coerce.UnsafeRefl -> GTI

instance SingletonConstructor SName

{-| Convert a singleton into a typeclass instance.

    In other words, use an t`SName` value as material evidence of a `KnownName`. -}
withSName :: forall sym r. SName sym -> (KnownName sym => r) -> r
withSName =
    Unsafe.Coerce.unsafeCoerce
        @(Text.Text -> (forall sym2. SName sym2 -> r) -> r)
        @(SName sym -> (KnownName sym           => r) -> r)
        withSomeSName

{-| Convert an arbitrary text into an t`SName`. -}
withSomeSName :: Text.Text -> (forall sym. SName sym -> r) -> r
withSomeSName bytes fn = fn (UnsafeSName bytes)

{-| Extract the text value from an t`SName`. -}
fromSName :: SName sym -> Text.Text
fromSName (UnsafeSName s) = s

{-| Construct an t`SName` from an arbitrary text. -}
toSomeSName :: Text.Text -> Some SName
toSomeSName s = Some (UnsafeSName s)

data KnownNameEvidence sym = KnownName sym => KnownNameEvidence

{-| An explicitly bidirectional pattern synonym that converts between t`SName` values and `KnownName` constraints. -}
pattern SName ::
    forall sym. () =>
    (KnownName sym) =>
    SName sym
pattern SName <- (flip withSName (KnownNameEvidence @sym) -> KnownNameEvidence)
  where
    SName = nameSing

{-# COMPLETE SName #-}
