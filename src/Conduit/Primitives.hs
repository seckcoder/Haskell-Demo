module Conduit.Primitives where

import Data.Conduit
import Control.Monad.IO.Class

source :: Source IO Int
source = do
    yield 1
    yield 2
    yield 3
    yield 4


conduit :: Conduit Int IO String
conduit = do
    mi1 <- await
    mi2 <- await
    case (mi1, mi2) of
        (Just i1, Just i2) -> do
            yield $ show (i1,i2)
            leftover i2
            conduit
        _ -> return ()

sink :: Sink String IO ()
sink = do
    mstr <- await
    case mstr of
        Nothing -> return ()
        Just str -> do
            liftIO $ putStrLn str
            sink

-- my version of source list
sourceList1 :: (Monad m) => [a] -> Source m a 
sourceList1 [] = return () -- return unit since type Source m a = ConduitM () a m ()
sourceList1 (a:as) = do
    yield a
    sourceList1 as


-- sink using awaitForever
sink1 :: Sink String IO ()
sink1 = awaitForever1 (liftIO . putStrLn)

-- my own version of awaitForever
awaitForever1 :: (Monad m) => (i -> Sink i m r) -> Sink i m ()
awaitForever1 f = do
    mv <- await
    case mv of
        Nothing -> return ()
        Just v -> do
            f v
            awaitForever1 f

main :: IO ()
main = do
    (sourceList1 [1..4]) =$ conduit $$ sink1
