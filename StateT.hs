module StateT where

newtype StateT s m h = StateT
  { runStateT :: s -> m (h, s)
  }

instance Functor m => Functor (StateT s m) where
  fmap f (StateT run) = StateT $ fmap (\(h, s) -> (f h, s)) . run

instance Monad m => Applicative (StateT s m) where
  pure = return
  StateT runF <*> StateT runX =
    StateT $ \s -> do
      (f, s') <- runF s
      (x, s'') <- runX s'
      return (f x, s'')

instance Monad m => Monad (StateT s m) where
  return x = StateT $ \s -> return (x, s)
  StateT run >>= f =
    StateT $ \s -> do
      (x, s') <- run s
      runStateT (f x) s'

execStateT :: Functor m => StateT s m h -> s -> m s
execStateT st s = snd <$> runStateT st s

getState :: Monad m => StateT s m s
getState = StateT $ \s -> return (s, s)

updateState :: Monad m => (s -> s) -> StateT s m ()
updateState f = StateT $ \s -> return ((), f s)

lift :: Monad m => m h -> StateT s m h
lift m = StateT $ \s -> (\x -> (x, s)) <$> m
