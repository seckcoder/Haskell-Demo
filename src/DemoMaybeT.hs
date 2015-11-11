module DemoMaybeT where

import Control.Monad.Trans.List
import Control.Monad.IO.Class
import Control.Monad

type Words = [String]

type Sentences = [Maybe Words]

gens1 :: Sentences
gens1 = return Nothing

gens2 :: Sentences
gens2 = return $ Just ["abc"]

main = print $ map (mplus Nothing) gens2