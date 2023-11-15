import Data.Char

map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x : map' f xs

doubleElems = map' (*2)
sqrElems = map' (^2)
lowerCase = map' toLower