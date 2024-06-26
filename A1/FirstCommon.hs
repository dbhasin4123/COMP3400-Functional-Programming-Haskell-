module FirstCommon (firstCommon) where


import           Prelude hiding (filter, fmap, foldl, foldr, liftA2, map, mapM,
                          mapM_, pure, replicate, return, reverse, sequence,
                          sequenceA, unzip, zip, zip3, zipWith, (*>), (<$),
                          (<$>), (<*), (<*>), (>>), (>>=))

firstCommon :: Eq a => [a] -> a
firstCommon = fst.maxSnd.fd

-- max element of list of tuples by 2nd position
maxSnd :: [(a, Integer)] -> (a, Integer)
maxSnd [(x,n)] = (x,n)
maxSnd ((x,n):xs)
    | n > m     = (x,n)
    | otherwise = (y,m)
  where
    (y, m) = maxSnd xs

-- frequency dictionary
fd :: Eq a => [a] -> [(a, Integer)]
fd [] = []
fd (x:xs) = join x $ fd $ xs

-- add/update frequency dictionary for one item
join :: Eq a => a -> [(a, Integer)] -> [(a, Integer)]
join x [] = [(x,1)]
join x ((y,n):ys)
    | x == y    = ys ++ [(y,n+1)]
    | otherwise = (y,n):(join x ys)