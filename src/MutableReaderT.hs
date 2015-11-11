module MutableReaderT where

import Data.Vector
import Control.Monad.Reader
import qualified Data.Vector.Mutable as VM
import Control.Monad.ST

type MyMon3 s = ReaderT (VM.MVector s Int) (ST s)

data Tree a = Null | Node (Tree a) a (Tree a) deriving Show

main :: IO ()
main = do 
    print $ runTraverse (Node Null 5 Null)

runTraverse :: Tree Int -> Vector Int
runTraverse t = runST $ do
        emp <- VM.replicate 7 0
        runReaderT (traverse t) emp
        Data.Vector.freeze emp

traverse :: Tree Int -> MyMon3 s ()
traverse Null = return ()
traverse (Node l v r) = do
    d <- ask
    a <- lift $ VM.read d v
    lift $ VM.write d v (a + 1)