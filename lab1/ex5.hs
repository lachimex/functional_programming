sgn :: Int -> Int
sgn n
    | n < 0 = -1
    | n == 0 = 0
    | otherwise = 1

absInt :: Int -> Int
absInt x = if x >= 0 then x else -x

min2Int :: (Int, Int) -> Int
min2Int (a, b) = if a > b then b else a

min3Int :: (Int, Int, Int) -> Int
min3Int (a, b, c) = min2Int (min2Int(a, b), min2Int(a, c))

toUpper :: Char -> Char
toUpper c = let x = fromEnum c
            in if x >= 97 && x <= 122 then toEnum (x - 32) :: Char else c

toLower :: Char -> Char
toLower c = let x = fromEnum c
            in if x >= 65 && x <= 90 then toEnum (x + 32) :: Char else c

isDigit :: Char -> Bool
isDigit c = let x = fromEnum c
            in x >= 48 && x <= 57 

charToNum :: Char -> Int
charToNum c = if isDigit c then fromEnum c - 48 else 0