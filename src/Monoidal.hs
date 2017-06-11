{-# LANGUAGE NoImplicitPrelude  #-}

-- define Monoidal as instance of Applicative

module Monoidal where
  import GHC.Base
  import Control.Applicative
  import System.IO

  class Applicative f => Monoidal f where
    unit :: f ()
    unit = pure ()
    (**) :: f a -> f b -> f (a, b)
    g ** h = (pure (,)) <*> g  <*> h

  instance Monoidal Maybe
  instance Monoidal []

  test :: IO ()
  test = do
    print $ (Just 1) ** (Just 2)
    print $ (Just 1) ** (Nothing:: Maybe Int)
    print $ [1,2,3] ** [4, 5]
    print $ [1,2,3] ** ([]:: [Int])