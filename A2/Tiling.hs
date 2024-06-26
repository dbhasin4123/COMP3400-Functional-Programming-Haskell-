module Tiling (tiling) where

type Triangle = [[Integer]]
type Row = [Integer]

tiling :: Integer -> [[Integer]]
tiling n = hTiling n [1..numTiles]
  where
    numTiles = div (4^n-1) 3

-- returns a tiling with zero in top triangle
hTiling :: Integer -> [Integer] -> [[Integer]]
hTiling 0 _ = [[0]]
hTiling n (x:xs) = connect t0 t1 t2 t3
  where
    [as, bs, cs, ds] = concat.map halves.halves $ xs
    t0 = hTiling (n-1) as
    t1 = replaceTop x $ hTiling (n-1) bs
    t2 = replaceTop x $ hTiling (n-1) cs
    t3 = replaceTop x $ hTiling (n-1) ds

-- replace top zero with different nubmer
replaceTop :: Integer -> Triangle -> Triangle
replaceTop k (xs:xss) = [k]:xss

-- break a list in half
halves :: [a] -> [[a]]
halves xs = toList $ splitAt (quot (length xs) 2) xs 
    where
        toList (x,y) = [x,y]

-- connect four triangles into one
connect :: Triangle -> Triangle -> Triangle -> Triangle -> Triangle
connect t0 t1 t2 t3 = t0 ++ zipWith3 (\xs ys zs -> xs++ys++zs) t1' t2' t3'
  where
    t1' = rotateCW t1
    t2' = flipHoriz t2
    t3' = rotateAC t3

-- get a column
strip :: Triangle -> Row
strip [[x]] = [x]
strip (xs:xss) = (head xs):rest
  where
    rest = foldr (++) [] $ map (\(x:y:zs) -> [y,x]) xss

-- strip colum
peel :: Triangle -> Triangle
peel [[x]] = []
peel (xs:xss) = map(\(x:y:zs) -> zs) xss

-- horiz flip
flipHoriz :: Triangle -> Triangle
flipHoriz = reverse

-- vertical flip
flipVert :: Triangle -> Triangle
flipVert = map reverse

-- clockwise rotation, top point becomes bottom RIGHT point
rotateCW :: Triangle -> Triangle
rotateCW = flipVert.rotateAC

-- anticlockwise rotation, top point becomes bottom LEFT point
rotateAC :: Triangle -> Triangle
rotateAC [[x]] = [[x]]
rotateAC xss = (rotateAC.peel) xss ++ [strip xss]