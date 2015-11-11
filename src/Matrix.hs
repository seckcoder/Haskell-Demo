module Matrix where

import Data.Matrix


main = let mat = fromLists [[1,2], [3,4]]
           in print $ mat ! (1,2)