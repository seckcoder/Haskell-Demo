module Vector2 where

import qualified Data.Vector.Mutable as MV
import Control.Monad.ST

type MVec2D s a = MV.MVector s (MV.MVector s a)



foo :: ST s Int
foo = do
    mv <- MV.replicate 10 5
    v <- MV.read mv 0
    return v

init2d :: ST s (MV.MVector s (MV.MVector s Int))
init2d = MV.replicateM 10 (MV.replicate 10 0)