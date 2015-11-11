-- | Main entry point to the application.
module Conduit.Conduit1 where

import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as CB

-- produce a stream of data sent to downstream
source :: Source IO Int
source = CL.sourceList [1..4]

-- consume a stream of string and produce a return value
sink :: Sink String IO ()
sink = CL.mapM_ putStrLn

conduit :: Conduit Int IO String
conduit = CL.map show

multiply2 :: Conduit Int IO Int
multiply2 = CL.map (* 2)

-- | The main entry point.
main :: IO ()
main = do
    source $$ (multiply2 =$= conduit) =$ sink
    source $$ (multiply2 =$= conduit =$ sink)
    source $$ (multiply2 =$ (conduit =$ sink))
