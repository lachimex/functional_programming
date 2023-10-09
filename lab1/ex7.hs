import Data.Bool (bool)
not' :: Bool -> Bool
not' True = False
not' False = True

isItTheAnswer :: String -> Bool
isItTheAnswer "Love" = True
isItTheAnswer _      = False

or' :: (Bool, Bool) -> Bool
or' (a, b) | not a && not b = False
           | otherwise      = True

and' :: (Bool, Bool) -> Bool
and' (a, b) | a && b = True
            | otherwise    = False
            
nand' :: (Bool, Bool) -> Bool
nand' (a, b)= not'(and'(a, b))

xor' :: (Bool, Bool) -> Bool
xor' (a, b) | a && b = False
            | not a && not b = False
            | otherwise      = True