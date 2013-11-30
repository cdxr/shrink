module Test.QuickCheck.Shrink where

import Control.Applicative

--import Control.Comonad

import Test.QuickCheck


data Shrink a = Shrink [a] a
    deriving (Show, Eq, Ord)

runShrink :: Shrink a -> [a]
runShrink (Shrink xs _) = xs

shrinks :: (a -> [a]) -> a -> Shrink a
shrinks f x = Shrink (f x) x

shrink' :: (Arbitrary a) => a -> Shrink a
shrink' = shrinks shrink


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
