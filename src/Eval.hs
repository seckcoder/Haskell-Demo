module Eval (
    Eval,
    Exp,
    Value,
    Env,
    runEval,
    eval
    ) where

-- | Code from "Monad Transformers Step by Step by Martin Grabmuller"

import Control.Monad.State.Class (MonadState(..))
import Control.Monad.Reader.Class (MonadReader(..))
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.Error (ErrorT, runErrorT)
import Control.Monad.Trans.Writer (WriterT, runWriterT)
import Control.Monad.Trans.State (StateT, runStateT)
import qualified Data.HashMap.Strict as H

data Exp = Lit Integer
         | Var String
         | Plus Exp Exp
         | Abs String Exp
         | App Exp Exp
         deriving (Show)

data Value = IntVal Integer
           | FunVal Env String Exp
           deriving (Show)

type Env = H.HashMap String Value

type Eval a = ReaderT Env (ErrorT String (WriterT [String] (StateT Integer IO))) a


runEval env st ev =
    runStateT (runWriterT (runErrorT (runReaderT ev env))) st

tick :: (Num s, MonadState s m) => m ()
tick = do st <- get
          put (st+1)

eval :: Exp -> Eval Value
eval (Lit i) = do tick
                  liftIO $ print i
                  return $ IntVal i
eval (Var n) = do tick
                  env <- ask
                  case H.lookup n env of
                      Nothing -> throwError ("unbound variable" ++ n)
                      Just val -> return val

eval (Plus e1 e2) = do tick
                       e1' <- eval e1
                       e2' <- eval e2
                       case (e1', e2') of
                           (IntVal i1, IntVal i2) ->
                               return $ IntVal (i1+i2)
                           _ -> throwError "type error in addition"

eval (Abs n e) = do tick
                    env <- ask
                    return $ FunVal env n e

eval (App e1 e2) = do tick
                      val1 <- eval e1
                      val2 <- eval e2
                      case val1 of
                          FunVal env' n body ->
                              local (const (H.insert n val2 env'))
                                (eval body)
                          _ -> throwError "type error in application"