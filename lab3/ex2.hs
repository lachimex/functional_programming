sum' :: Num a => [a] -> a
sum' []     = 0
sum' (x:xs) = x + sum' xs

sumSqr' :: Num a => [a] -> a
sumSqr' [] = 0
sumSqr' (x:xs) = x^2 + sumSqr' xs

sumWith :: Num a => (t -> a) -> [t] -> a
sumWith f [] = 0
sumWith f (x:xs) = f x + sumWith f xs

sum'' = sumWith id
sumSqr = sumWith(^2)
sumCube = sumWith(^3)
sumAbs = sumWith abs
listLength = sumWith (\x -> 1)

a = foldr (+) 0 . map(^2) . filter ((==1) . (`mod` 3) ). map(+1) . map(*2) $ [0..7]

f1 :: (Integer -> b) -> Integer -> [b]
f1 = \g -> map g . (\x -> map ($ x) [id, (+1), (+2)])

fun :: String -> Int
fun = length . filter ((>2) . length) . map (filter (`elem` vowels)) . words
      where vowels = "aeiouy"