module StateT where

newtype StateT s a = StateT
  { runStateT :: s -> (s, a)
  }

instance Functor (StateT s) where
  fmap f (StateT run) = StateT $ fmap f . run

instance Applicative (StateT s) where
  pure x = StateT $ \s -> (s, x)
  StateT runF <*> StateT runX =
    StateT $ \s ->
      let (s', f) = runF s
          (s'', x) = runX s'
       in (s'', f x)

instance Monad (StateT s) where
  StateT run >>= f =
    StateT $ \s ->
      let (s', x) = run s
       in runStateT (f x) s'

getState :: StateT s s
getState = StateT $ \s -> (s, s)
