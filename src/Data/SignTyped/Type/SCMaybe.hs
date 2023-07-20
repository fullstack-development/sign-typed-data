{-| This module defines conditionally present polymorphic values. Unlike Prelude's `Maybe`, the condition is given at
the type level. If the value is present, it can be of any type, but it must support the given constraint.

    In the @sign-typed-data@ library, it's used for the fields of `Data.SignTyped.EIP712Domain.EIP712Domain`:
depending on the application, some fields may be absent, and those that are present might use an application-specific
representation, provided that there is a way to encode it into a Solidity object.

    The @S@ letter in this module stands for "singleton", the same as in `GHC.TypeLits.SNat`, `GHC.TypeLits.SChar` etc.

    The @C@ letter stands for "constrained", and refers to the fact that a non-empty type must support the given
constraint.

    The @F@ letter in `FMaybe` simply means "type family" (as opposed to a GADT or a normal data). -}
module Data.SignTyped.Type.SCMaybe
    ( SCMaybe (..)
    , KnownCMaybe (..)
    , withCMaybe
    , FMaybe (..)
    , fmaybe
    )
where

import Data.Kind

{-| A singleton marker for a constrained type-level `Maybe`.

    An explicit forall on the @SCJust@ constructor allows the existential @a@ to be pattern-matched with a
    type-application pattern, for example:

    @
        mbNatVal :: SCMaybe KnownNat mbx -> Maybe Integer
        mbNatVal SCNothing = Nothing
        mbNatVal (SCJust \@x) = Just (`GHC.TypeLits.natVal` \@x undefined)
    @ -}
type SCMaybe :: (k -> Constraint) -> Maybe k -> Type
data SCMaybe (c :: k -> Constraint) (m :: Maybe k) where
    SCNothing :: SCMaybe c 'Nothing
    SCJust :: forall a c. c a => SCMaybe c ('Just a)

{-| A class to automatically produce `SCMaybe`s . -}
class KnownCMaybe c m where
    cmaybeSing :: SCMaybe c m

instance KnownCMaybe c 'Nothing where
    cmaybeSing = SCNothing

instance c a => KnownCMaybe c ('Just a) where
    cmaybeSing = SCJust

{-| A destructor for a context-provided `SCMaybe`, analogous to `maybe`, `either` and `Data.Bool.bool`. -}
withCMaybe ::
    forall c m r.
    (KnownCMaybe c m) =>
    (m ~ 'Nothing => r) ->
    (forall a. (m ~ 'Just a, c a) => r) ->
    r
withCMaybe onSCNothing onSCJust =
    case cmaybeSing @c @m of
        SCNothing -> onSCNothing
        SCJust -> onSCJust



{-| The type family for a conditionally present value. -}
data family FMaybe (m :: Maybe Type) :: Type

data instance FMaybe 'Nothing = FNothing

newtype instance FMaybe ('Just a) = FJust { getFJust :: a }

{-| A destructor for an `FMaybe`, which uses a `KnownCMaybe` from the context to determine the actual state of
the value (present or absent) and retrieve the required constraint in the present case. -}
fmaybe ::
    forall c m r.
    (KnownCMaybe c m) =>
    (m ~ 'Nothing => r) ->
    (forall a. (m ~ ('Just a), c a) => a -> r) ->
    FMaybe m ->
    r
fmaybe onFNothing onFJust =
    case cmaybeSing @c @m of
        SCNothing -> \FNothing -> onFNothing
        SCJust -> \(FJust x) -> onFJust x

instance (KnownCMaybe Show m) => Show (FMaybe m) where
    showsPrec d =
        fmaybe @Show
            (showString "FNothing")
            (\x ->
                showParen (d > 10) $
                    showString "FJust " .
                    showsPrec 11 x
            )
