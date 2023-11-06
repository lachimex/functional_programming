f1 = \x -> x + 2

f2 = \x -> \y -> sqrt (x^2 + y^2)

f3 :: Integer -> Integer -> Integer -> Double
f3 = \x y z -> sqrt (fromInteger(x^2 + y^2 + z^2))

g1 = \x -> 2 * x

g2 = \x -> x * 2

g3 = \x -> 2^x

g4 = \x -> x^2

g5 = \x -> 2/x

g6 = \x -> x/3

g7 = \x -> 4-x

f7 = \x -> even x

f8 = \x -> 2 * sqrt(x)^3 * (sqrt(x) + 1)

f9 = \x -> if x == 1 then 3 else 0