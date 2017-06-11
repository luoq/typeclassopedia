module ApplicativeTest where
  sequenceAL :: Applicative f => [f a] -> f [a]
  sequenceAL [] = pure []
  sequenceAL (x:xs) = (++) <$> ((fmap pure) x) <*> (sequenceAL xs)
  -- sequenceAL (x:xs) = (pure (++)) <*> ((fmap pure) x) <*> (sequenceAL xs)

  test :: IO ()
  test = do
    print $ (sequenceAL [] :: Maybe [Int])
    print $ sequenceAL [Just 12]
    print $ sequenceAL [Just 12, Just 3]
    print $ sequenceAL [Just 12, Just 3, Just 4]
    print $ sequenceAL [Just 12, Nothing, Just 3]
    print $ (sequenceAL [] :: [[Int]])
    print $ sequenceAL [[2], [3, 4]]
    print $ sequenceAL [[0,1], [2], [3, 4]]
    print $ sequenceAL [[0,1], [], [3, 4]]