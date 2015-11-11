module Conduit.MonadicChaining where


import Data.Conduit
import qualified Data.Conduit.List as CL

multiplyFirst :: (Monad m, Num a) => Conduit a m a
multiplyFirst = do
    mv0 <- await
    case mv0 of
        Just v0 -> CL.map (* v0)
        Nothing -> return ()


main = do
    CL.sourceList [2..8] $= multiplyFirst $$ CL.mapM_ (putStrLn . show)
