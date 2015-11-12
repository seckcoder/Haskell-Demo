{-# LANGUAGE FlexibleInstances #-}
module Main where

import Control.Monad
import Data.Monoid
import Control.Monad.Trans.List

a :: ListT Maybe Int
a = return 3


b :: ListT Maybe Int
b = return 4

main = print $ runListT $ a `mplus` b