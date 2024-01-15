import Control.Monad
import Data.Map.Internal.Debug (validsize)
xs1 :: [(Int,Int,Int)]
xs1 = [ (x,y,z) | let xs = [1,2], x <- xs, y <- xs, z <- xs ]

doXs1 :: [(Int,Int,Int)]
doXs1 = do
  let xs = [1,2]
  x <- xs
  y <- xs
  z <- xs
  return (x,y,z)

xs2 :: [(Int,Int,Int)]
xs2 = [ (x,y,z) | let xs = [1..3], x <- xs, y <- xs, z <- xs, x > y && y > z ]

doXs2 :: [(Int,Int,Int)]
doXs2 = do
  let xs = [1..3]
  x <- xs
  y <- xs
  z <- xs
  guard $ x > y && y > z
  return (x,y,z)

doXs2' :: [(Int,Int,Int)]
doXs2' = do
  let xs = [1..3]
  x <- xs
  y <- xs
  z <- xs
  if x > y && y > z
    then return (x,y,z)
    else []

joinArray :: [[a]] -> [a]
joinArray [[]] = []
joinArray [x:xs] = x:xs

triples n = do
    let vals = [1..n]
    a <- vals
    b <- vals
    c <- vals
    guard $ (a^2 + b^2) `mod` c^2 == 1
    return [(a, b, c)]

sumLog :: [Double] -> Maybe Double
sumLog [] = Just 0
sumLog (x:xs) = 
  if x <= 0 then Nothing
  else do
    s <- sumLog xs
    return $ s + log x