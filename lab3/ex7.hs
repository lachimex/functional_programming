import Data.Char

filter' :: (a -> Bool) -> [a] -> [a]
filter' p [] = []
filter' p (x:xs)
 | p x = x : filter' p xs
 | otherwise = filter' p xs

onlyEven = filter' even
onlyOdd = filter' odd
onlyUpper = filter' isUpper