sqr :: Num a => a -> a
sqr x = x * x

vec2DLen :: (Double, Double) -> Double
vec2DLen (x, y) = sqrt (x^2 + y^2)

vec3DLen :: (Double, Double, Double) -> Double
vec3DLen (x, y, z) = sqrt (x^2 + y^2 + z^2)

swap :: (Int, Char) -> (Char, Int)
swap (a, b) = (b, a)

threeEqual :: (Int, Int, Int) -> Bool
threeEqual (x, y, z) = x == y && y == z && x == z

triangleField :: Floating a => (a, a, a) -> a
triangleField (a, b, c) = sqrt(p * (p - a) * (p - b) * (p -c)) 
    where p = (a + b + c) / 2