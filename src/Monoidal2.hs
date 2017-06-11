{-# LANGUAGE NoImplicitPrelude  #-}

-- define Monoidal as instance of Applicative

module Monoidal2 where
  import GHC.Base
  import GHC.Num
  import Data.Tuple
  import System.IO

  class Functor f => Monoidal2 f where
    unit :: f ()
    (**) :: f a -> f b -> f (a, b)

  class Monoidal2 f => Applicative2 f where
    pure_ :: a -> f a
    pure_ x = fmap (const x) unit
    link :: f (a -> b) -> f a  -> f b
    g `link` h = fmap (uncurry ($)) (g ** h)

  instance Monoidal2 Maybe where
    unit = Just ()
    (Just x) ** (Just y) = Just (x, y)
    _ ** _ = Nothing

  instance Applicative2 Maybe

  instance Monoidal2 [] where
    unit = [()]
    x ** y = [(a, b)| a<-x, b <- y]

  instance Applicative2 []

  test:: IO ()
  test = do
    print $ (Just 1) ** (Nothing :: Maybe Int)
    print $ (Just 1) ** (Just 2)
    print $ (pure_ (\x -> 2 * x + 1)) `link` (Just 1)
    print $ [1,2,3] ** ([]:: [Int])
    print $ (pure_ (\x -> 2 * x + 1)) `link` [1,2,3]