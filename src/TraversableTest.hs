{-# LANGUAGE InstanceSigs #-}
module TraversableTest where
  import Data.Foldable
  import Data.Monoid
  data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving Show

  instance Functor Tree where
    fmap g (Leaf x) = Leaf (g x)
    fmap g (Node l x r) = Node (fmap g l) (g x) (fmap g r)
  
  instance Foldable Tree where
    fold :: Monoid m => Tree m -> m
    fold (Leaf x) = x
    fold (Node l x r) = (fold l) `mappend` x `mappend` (fold r)

    foldMap :: Monoid m => (a -> m) -> Tree a -> m
    foldMap g x = fold (fmap g x)
  
  instance Traversable Tree where
    sequenceA :: (Applicative f) => Tree (f a) -> f (Tree a)
    sequenceA (Leaf x) = fmap Leaf x
    sequenceA (Node l x r) = Node <$> (sequenceA l) <*> x <*> (sequenceA r)

  test :: IO ()
  test = do
    print $ tree3
    print $ fmap (+1) tree3
    print $ getSum $ foldMap Sum tree3
    print $ getProduct $ foldMap Product tree3
    print $ length tree3
    print $ length $ traverse (\x -> [x, x+1, x+2])tree3
    where
      tree1 = Node (Leaf 1) 2 (Leaf 3)
      tree2 = Node (Leaf 5) 6 (Leaf 7)
      tree3 = Node tree1 4 tree2
