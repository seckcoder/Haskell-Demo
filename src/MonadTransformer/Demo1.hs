{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module MonadTransformer.Demo1 where


import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, ask)
import Control.Monad.Trans.State (StateT)
import Control.Monad.State (State)
import Control.Monad.State.Class (MonadState(..))
import Control.Monad.Identity (Identity)
import qualified Data.HashMap.Lazy as H


data Exp = Lit Integer
         | Val String
         | Abs String Exp
         | App Exp Exp

data Value = IntVal Integer
           | FunVal Env String Exp

type Env = H.HashMap String Value

type Eval a = ReaderT Env (StateT Integer Identity) a

tick :: (Num s, MonadState s m) => m ()
tick = do i <- get
          put (i+1)

eval :: Exp -> Eval Value
eval (Lit i) =  do tick
                   return $ IntVal i

eval (Val n) = do env <- ask
                  case H.lookup n env of
                      Just v -> return v
                      Nothing -> error ("can't find:" ++ n)