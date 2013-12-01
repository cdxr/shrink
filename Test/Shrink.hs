module Test.Shrink where

import Control.Applicative

--import Control.Comonad

import Test.QuickCheck


-- | A @Shrink a@ is a value of @a@ with a list of potential shrinks of
-- that value.
data Shrink a = Shrink [a] a
    deriving (Show, Eq, Ord)

-- | Extract the possible shrinks from a @Shrink a@.
runShrink :: Shrink a -> [a]
runShrink (Shrink xs _) = xs

-- | `shrinks` is similar to `shrink`, but returns a `Shrink` instead of a
-- list.
shrinks :: (Arbitrary a) => a -> Shrink a
shrinks = shrinksWith shrink

-- | @shrinksWith f@ is like `shrinks` but uses the custom function @f@ to
-- perform the shrink.
shrinksWith :: (a -> [a]) -> a -> Shrink a
shrinksWith f x = Shrink (f x) x


instance Functor Shrink where
    fmap f (Shrink xs x) = Shrink (map f xs) (f x)

instance Applicative Shrink where
    pure = Shrink []
    Shrink fs f <*> Shrink xs x =
        Shrink (map ($ x) fs ++ map f xs) (f x)

{-
-- This is commented out to avoid a dependency on the comonad package

instance Comonad Shrink where
    extract (Shrink _ x) = x
    duplicate s@(Shrink xs _) = Shrink (map (Shrink xs) xs) s
-}
