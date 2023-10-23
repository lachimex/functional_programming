fiveToPower_ :: Integer -> Integer
fiveToPower_ = (5^) -- fiveToPower_ 3 = 125

_ToPower5 :: Num a => a -> a
_ToPower5 = (^5) -- _ToPower5 2 = 32

subtrNFrom5 :: Num a => a -> a
subtrNFrom5 = (5-) -- subtrNFrom5 3 = 2

subtr5From_ :: Num a => a -> a
subtr5From_ = (+(-5)) -- subtr5From_ 6 = 1

xToPowerYSubstractZ x y z= x^y - z

flip2 :: (t1 -> t2 -> t3) -> t2 -> t1 -> t3
flip2 f a b = f b a 

flip3 :: (t1 -> t2 -> t3 -> t4) -> t3 -> t2 -> t1 -> t4
flip3 f a b c = f c b a 