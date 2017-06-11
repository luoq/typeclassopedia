{-# LANGUAGE InstanceSigs, FlexibleContexts, UndecidableInstances #-}

module MonadTest where
  data Free f a = Var a
    | Node (f (Free f a))
  
  instance ((Applicative f), (Applicative (Free f))) => Monad (Free f) where
    return :: a -> (Free f a)
    return = Var
    (>>=) :: (Free f a) -> (a -> (Free f b)) -> (Free f b)
    (Var x) >>= g = g x
    (Node x) >>= g = Node (fmap (>>= g) x)

  -- (>>=) :: m a -> (a -> m b) -> m b
  -- id :: m a -> m a
  -- return :: a -> m a
  join :: (Monad m) => m (m a) -> m a
  join x = x >>= id

  fmap' :: (Monad m) => (a -> b) -> m a -> m b
  fmap' g x = x >>= (return . g)

  -- define <*>
  -- think do notation
  ap :: Monad m => m (a -> b) -> m a -> m b
  ap g x = g >>= (\u -> x >>= (return . u))

  -- m >>= (\x -> k x >>= h)  =  (m >>= k) >>= h
  -- (>=>) :: Monad m :: (a -> m b) -> (b -> m c) -> (a -> m c)
  -- g (>=>) h = \x -> (g x) >>= h

  test :: IO ()
  test = print 42