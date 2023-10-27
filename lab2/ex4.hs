import Data.Char (toUpper)
import System.Win32 (xBUTTON1)
isPalindrome :: [Char] -> Bool
isPalindrome s = s == reverse s


getElemAtIdx :: Int -> [a] -> a
getElemAtIdx i t = let x = drop (i-1) t
                    in head x

capitalize :: [Char] -> [Char]
capitalize s = toUpper (head s) : tail s

--let x = [(a, b, c) | a <- [1..100], b <- [1..100], c <- [1..100], a^2 + b^2 == c^2]

isPrime :: Integral t => t -> Bool
isPrime n = null([i | i <- [2..n-1], n `mod` i == 0])

howManyPrime :: [Integer]
howManyPrime = [x | x <- [2..10000], isPrime x]

primes :: [Int]
primes = eratoSieve [2..]
 where
   eratoSieve :: [Int] -> [Int]
   eratoSieve (p : xs) = p : eratoSieve [x | x <- xs, x `mod` p /= 0]

isPrime2 :: Int -> Bool
isPrime2 n
    | n <= 1 = False
    | otherwise = n `elem` takeWhile (<= n) primes

howManyPrimes :: Int -> Int
howManyPrimes n = length (takeWhile (<= n) primes)


allEqual :: Eq a => [a] -> Bool
allEqual [] = True       
allEqual [x] = True      
allEqual (x:y:rest)      
    | x == y = allEqual (y:rest)
    | otherwise = False