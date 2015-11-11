module Conduit.Termination where

import Data.Conduit
import Control.Monad.IO.Class
import qualified Data.Conduit.List as CL

-- Termination

source :: Source IO Int
source = do
    liftIO $ putStrLn "source: yielding 1"
    yield 1
    liftIO $ putStrLn "source: yielding 2"
    yield 2
    liftIO $ putStrLn "source: yielding 3"
    yield 3

conduit :: Conduit Int IO Int
conduit = do
    liftIO $ putStrLn "conduit calling await"
    mx <- await
    case mx of
        Nothing -> liftIO $ putStrLn "Nothing left, exiting"
        Just x -> do
            liftIO $ putStrLn $ "conduit yielding " ++ show x
            yield x
            conduit

sink 0 = liftIO $ putStrLn "sink is finished, terminating"
sink i = do
    liftIO $ putStrLn $ "sink: still waiting for " ++ show i
    mx <- await
    case mx of
        Nothing -> liftIO $ putStrLn "sink: Nothing from upstream, exiting"
        Just x -> do
            liftIO $ putStrLn $ "sink received: " ++ show x
            sink (i - 1)


source1 :: Source IO Int
source1 =
    loop 1
  where
    loop i = do
        yieldOr i $ putStrLn $ "Terminated when yielding: " ++ show i
        loop $ i + 1

main = source1 $$ CL.isolate 7 =$ CL.mapM_ print
