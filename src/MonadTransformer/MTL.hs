{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module MonadTransformer.MTL where

import Control.Monad.Identity (Identity)
import qualified Data.HashMap.Strict as H

-- Monad Transformer By Yourself


class MonadTrans t where
    -- | lift a computation from the argument monad to the constructed monad
    lift :: (Monad m) => m a -> t m a

newtype ReaderT r m a = ReaderT {runReaderT :: r -> m a}

type Reader r = ReaderT r Identity

instance MonadTrans (ReaderT r) where
    lift m = ReaderT $ \r -> m

newtype StateT s m a = StateT {runStateT :: s -> m (a,s) }

type State s = StateT s Identity

instance MonadTrans (StateT s) where
    lift m = StateT $ \s -> m >>= \a -> return (a,s)

class (Monad m) => MonadState s m | m -> s where
    get :: m s
    put :: s -> m ()

instance (Monad m) => Monad (StateT s m) where
    return a = StateT $ \s -> return (a,s)
    m >>= f = StateT $ \s -> do
        (a, s) <- runStateT m s
        runStateT (f a) s
    
instance (Monad m) => MonadState s (StateT s m) where
    get = StateT $ \s -> return (s,s)
    put s = StateT $ \_ -> return ((), s)

class (Monad m) => MonadReader r m | m -> r where
    ask :: m r

instance (Monad m) => Monad (ReaderT r m) where
    return a = ReaderT $ \r -> return a
    m >>= f = ReaderT $ \r -> do
        a <- runReaderT m r
        runReaderT (f a) r

instance (Monad m) => MonadReader r (ReaderT r m) where
    ask = ReaderT $ \r -> return r

-- For a Monad `m` having `MonadState` constraint,
-- it is still a `MonadState` after transformed by `ReaderT`
instance (MonadState s m) => MonadState s (ReaderT r m) where
    get = lift get
    put = lift . put

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