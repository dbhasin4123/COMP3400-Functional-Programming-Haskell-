module Words (countWays) where


import           Prelude hiding (filter, fmap, foldl, foldr, liftA2, map, mapM,
                          mapM_, pure, replicate, return, reverse, sequence,
                          sequenceA, unzip, zip, zip3, zipWith, (*>), (<$),
                          (<$>), (<*), (<*>), (>>), (>>=))

countWays :: String -> String -> Integer
countWays _ ""  = 1
countWays "" _  = 0
countWays (c:cs) (d:ds)
    | c == d    = countWays cs ds + countWays (cs) (d:ds)
    | otherwise = countWays (cs) (d:ds)