module Data.SignTyped.Type.HList
    ( HList (..)
    , hmap
    , htraverse
    , hfoldr
    , hfoldMap
    , hzip
    , hzipWith
    , htoList
    , hfromList
    , hsequenceSome
    , Product (..)
    )
where

import Control.Applicative
import Data.Functor.Product
import Data.SignTyped.Type.Some
import Data.Type.Equality

infixr 4 :/
{-| A heterogeneous list.

    The first parameter is a unary type constructor, which is applied to each of the elements of the second parameter
to obtain the full type of each of the `HList`'s elements.

    For example, a value of type @HList f \'[t1, t2, t3]@ contains three objects of types @(f t1, f t2, f t3)@. -}
data HList f ts where
    (:/) :: f t -> HList f rest -> HList f (t ': rest)
    HEnd :: HList f '[]

deriving instance (forall t. Show (f t)) => Show (HList f ts)

instance Semigroup (Some (HList f)) where
    Some (x :/ rest) <> ys = withSome (Some rest <> ys) (Some . (x :/))
    Some HEnd <> ys = ys

instance Monoid (Some (HList f)) where
    mempty = Some HEnd

instance TestEquality f => TestEquality (HList f) where
    testEquality (a :/ as) (b :/ bs) = do
        Refl <- testEquality a b
        Refl <- testEquality as bs
        Just Refl
    testEquality HEnd HEnd =
        Just Refl
    testEquality _ _ =
        Nothing

{-| Heterogeneous `map`. The mapping function is a natural transformation.

    For example:

    @
        hmap
            (maybe (Left "no value") Right
                :: forall t. Maybe t -> Either String t)
            (Just 10 :\/ Just "text"  :\/ Nothing \@Float  :\/ HEnd
                :: HList Maybe '[Int, String, Float])
      ==
            Right 10 :\/ Right "text" :\/ Left "no value" :\/ HEnd
                :: HList (Either String) '[Int, String, Float]

    @ -}
hmap :: (forall t. f t -> g t) -> HList f ts -> HList g ts
hmap f (x :/ rest) = f x :/ hmap f rest
hmap _ HEnd = HEnd

{-| Heterogeneous `traverse`.

    For example:

    @
        htraverse
            (maybe ([Left "no value"]) (\\x -> [Left "removed", Right x])
                :: forall t. Maybe t -> [Either String t])
            (Just 10         :\/ Just "text"    :\/ Nothing \@Float  :\/ HEnd
                :: HList Maybe '[Int, String, Float])
      ==
            [ Left "removed" :\/ Left "removed" :\/ Left "no value" :\/ HEnd
            , Left "removed" :\/ Right "text"   :\/ Left "no value" :\/ HEnd
            , Right 10       :\/ Left "removed" :\/ Left "no value" :\/ HEnd
            , Right 10       :\/ Right "text"   :\/ Left "no value" :\/ HEnd
            ]
                :: [HList (Either String) '[Int, String, Float]]

    @ -}
htraverse :: Applicative m => (forall t. f t -> m (g t)) -> HList f ts -> m (HList g ts)
htraverse f (x :/ rest) = liftA2 (:/) (f x) (htraverse f rest)
htraverse _ HEnd = pure HEnd

{-| Heterogeneous `foldr`.

    For example:

    @
        hfoldr
            (maybe ("n" ++) (\\_ -> ("j" ++))
                :: forall t. Maybe t -> String -> String)
            ""
            (Just 10 :\/ Just "text"  :\/ Nothing \@Float  :\/ HEnd
                :: HList Maybe '[Int, String, Float])
      ==
            "jjn"

    @ -}
hfoldr :: (forall t. f t -> r -> r) -> r -> HList f ts -> r
hfoldr f z (x :/ rest) = f x (hfoldr f z rest)
hfoldr _ z HEnd = z

{-| Heterogeneous `foldMap`. -}
hfoldMap :: Monoid m => (forall t. f t -> m) -> HList f ts -> m
hfoldMap f (x :/ rest) = f x <> hfoldMap f rest
hfoldMap _ HEnd = mempty

{-| Heterogeneous `zip`.

    For example:

    @
        hzip
            (Just 10         :\/ Just "text"    :\/ Nothing         :\/ HEnd
                :: HList Maybe '[Int, String, Float])
            (Const "a"       :\/ Const "b"      :\/ Const "c"       :\/ HEnd
                :: HList (Const String) '[Int, String, Float])
      ==
            Pair (Just 10) (Const "a") :\/ Pair (Just "text") (Const "b") :\/ Pair Nothing (Const "c") :\/ HEnd
                :: HList (Product Maybe (Const String)) '[Int, String, Float]

    @ -}
hzip :: HList f ts -> HList g ts -> HList (Product f g) ts
hzip (x :/ xrest) (y :/ yrest) = Pair x y :/ hzip xrest yrest
hzip HEnd HEnd = HEnd

{-| Heterogeneous `zipWith`.

    For example:

    @
        hzipWith
            ((\\ma eb -> maybeToList ma <> rightToList eb)
                :: forall t. Maybe t -> Either () t -> [t])
            (Just 10         :\/ Just "text"    :\/ Nothing         :\/ HEnd
                :: HList Maybe '[Int, String, Float])
            (Right 20        :\/ Left ()        :\/ Right 34.5      :\/ HEnd
                :: HList (Either ()) '[Int, String, Float])
      ==
            [10, 20] :\/ ["text"] :\/ [34.5] :\/ HEnd
                :: HList [] '[Int, String, Float]

    @ -}
hzipWith :: (forall t. f t -> g t -> h t) -> HList f ts -> HList g ts -> HList h ts
hzipWith f (x :/ xrest) (y :/ yrest) = f x y :/ hzipWith f xrest yrest
hzipWith _ HEnd HEnd = HEnd

{-| Convert an `HList` of `Const` into a normal list. -}
htoList :: HList (Const a) ts -> [a]
htoList (Const x :/ rest) = x : htoList rest
htoList HEnd = []

{-| Convert a normal list into an `HList` of `Const`. -}
hfromList :: [a] -> Some (HList (Const a))
hfromList [] = Some HEnd
hfromList (x : rest) = withSome (hfromList rest) (Some . (Const x :/))

{-| Convert a normal list of `Some`s into an `HList`.

    The name refers to the fact that, similar to `sequenceA`, the effect of this function can be seen as
a transposition of containers. -}
hsequenceSome :: [Some f] -> Some (HList f)
hsequenceSome [] = Some HEnd
hsequenceSome (Some x : rest) = withSome (hsequenceSome rest) (Some . (x :/))
