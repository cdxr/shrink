{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Test.QuickCheck.Shrink where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader

import Test.QuickCheck


-- | @Shrink r a@ is a computation that shrinks @a@ as a component of
-- a larger type @r@ (typically a record).
newtype Shrink r a = Shrink (ReaderT r [] a)
    deriving (Functor, Applicative, Alternative, Monad, MonadPlus)

runShrink :: Shrink r a -> r -> [a]
runShrink (Shrink m) = drop 1 . runReaderT m

shrinks :: (Arbitrary a) => (r -> a) -> Shrink r a
shrinks = shrinksWith shrink

shrinksWith :: (a -> [a]) -> (r -> a) -> Shrink r a
shrinksWith sf rf = Shrink $ do
    a <- asks rf
    lift $ a : sf a

shrink' :: (Arbitrary a) => a -> Shrink r a
shrink' = shrinks . const
