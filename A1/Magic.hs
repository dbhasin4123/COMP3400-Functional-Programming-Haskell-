module Magic (magic, condIncr, trim, enumerate) where

import           Prelude hiding (filter, fmap, foldl, foldr, liftA2, map, mapM,
                          mapM_, pure, replicate, return, reverse, sequence,
                          sequenceA, unzip, zip, zip3, zipWith, (*>), (<$),
                          (<$>), (<*), (<*>), (>>), (>>=))

magic :: (a -> (b -> c)) -> ([a] -> ([b] -> [c]))
magic _ [] _ = []
magic _ _ [] = []
magic f (x:xs) (y:ys) = (f x y):(magic f xs ys)

condIncr :: [Int] -> [Bool] -> [Int]
condIncr = magic (\x y -> if y then x+1 else x)

trim :: [a] -> [b] -> [a]
trim = magic (\x y -> x)

enumerate :: [a] -> [(Int, a)]
enumerate = magic (,) [0..]