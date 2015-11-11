{-# OPTIONS_GHC -fglasgow-exts #-}

module Nondeterminism.MonadNondt where

import Control.Monad
import Control.Monad.Trans
 
import Control.Monad.Identity
 
newtype NondetT m a
  = NondetT { foldNondetT :: (forall b. (a -> m b -> m b) -> m b -> m b) }

-- find one answer or fail
runNondetT :: (Monad m) => NondetT m a -> m a
runNondetT m = foldNondetT m (\x xs -> return x) (error "No solution found.")
 
instance (Functor m) => Functor (NondetT m) where
  fmap f (NondetT g) = NondetT (\cons nil -> g (cons . f) nil)
 
instance (Monad m) => Monad (NondetT m) where
  return a = NondetT (\cons nil -> cons a nil)
  m >>= k  = NondetT (\cons nil -> foldNondetT m (\x -> foldNondetT (k x) cons) nil)
 
instance (Monad m) => MonadPlus (NondetT m) where
  mzero         = NondetT (\cons nil -> nil)
  m1 `mplus` m2 = NondetT (\cons -> foldNondetT m1 cons . foldNondetT m2 cons)
 
instance MonadTrans NondetT where
  lift m = NondetT (\cons nil -> m >>= \a -> cons a nil)
 
newtype Nondet a = Nondet (NondetT Identity a) deriving (Functor, Monad, MonadPlus)
runNondet (Nondet x) = runIdentity (runNondetT x)
 
foldNondet :: Nondet a -> (a -> b -> b) -> b -> b
foldNondet (Nondet nd) cons nil =
   runIdentity $ foldNondetT nd (\x xs -> return (cons x (runIdentity xs))) (return nil)
 
option :: (MonadPlus m) => [a] -> m a
option = msum . map return

main = print $ runNondet $ do
    x <- option [1..10] :: Nondet Int
    guard (x >= 5)
    return x