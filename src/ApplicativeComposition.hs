{-# LANGUAGE InstanceSigs #-}
module ApplicativeComposition where
  data Compose f g x = Compose (f (g x)) deriving Show

  instance (Functor f, Functor g) => Functor (Compose f g) where
    fmap :: (a -> b) -> (Compose f g) a -> (Compose f g) b
    fmap g (Compose x) = Compose ((fmap (fmap g)) x)
  
  instance (Applicative f1, Applicative f2) => (Applicative (Compose f1 f2)) where
    pure :: a -> (Compose f1 f2) a
    pure a = Compose(pure (pure a))
    -- g :: (Compose f1 f2) a -> (Compose f1 f2) b
    -- g :: (f1 (f2 a))
    (<*>) :: (Compose f1 f2) (a->b) -> (Compose f1 f2) a -> (Compose f1 f2) b
    (Compose g) <*> (Compose x) = Compose(((pure (<*>)) <*> g) <*> x)
    -- g :: (f1 (f2 (a->b)))
    -- x :: (f1 (f2 a))
    -- target' :: (f1 (f2 b))
    -- g' ? ::  (f1 (f2 a -> f2 b))
    -- (<*>):: f2 (a -> b) -> (f2 a -> f2 b)
    -- pure (<*>) :: (f1 ((f2 (a->b)) -> (f2 a -> f2 b))
    -- (pure (<*>)) <*> g :: (f1 (f2 a -> f2 b))
    -- ((pure (<*>)) <*> g) <*> x :: (f1 (f2 b))

  test :: IO ()
  test = do
    print 12
    print $ Compose([Just (*2), Just(+1)]) <*> Compose([Just(1), Just(2)])
    print $ Compose([Just (*2), Nothing]) <*> Compose([Just(1), Just(2)])
    print $ Compose([[(*2), (*3)], [(+10), (+11)]]) <*> Compose [[1,2], [3,4]]
    print $ Compose([[(*2), (*3)], []]) <*> Compose [[1,2], [3,4]]