type X = Int
type Y = Int

data CartInt2DVec = MkCartInt2DVec X Y

xCoord :: CartInt2DVec -> Int
xCoord (MkCartInt2DVec x _) = x

yCoord :: CartInt2DVec -> Int
yCoord (MkCartInt2DVec _ y) = y

data Cart2DVec' a = MkCart2DVec' a a

xCoord' :: Cart2DVec' a -> a
xCoord' (MkCart2DVec' x _) = x

yCoord' :: Cart2DVec' a -> a
yCoord' (MkCart2DVec' _ y) = y

data Cart2DVec'' a = MkCart2DVec'' {x::a, y::a}
    deriving Show

-- sum type example (two constructors)
data List a = EmptyL | Cons a (List a) deriving Show

head' :: List a -> a
head' EmptyL      = error "head': the empty list has no head!"
head' (Cons x xs) = x

-- enum type example (special case of sum type)
-- data ThreeColors = Blue |
--                    White |
--                    Red

-- type ActorName = String

-- leadingActor :: ThreeColors -> ActorName
-- leadingActor Blue  = "Juliette Binoche"
-- leadingActor White = "Zbigniew Zamachowski"
-- leadingActor Red   = "Irene Jacob"

data Cart3DVec a = MkCart3DVec a a a

xCoord3D (MkCart3DVec a _ _) = a
yCoord3D (MkCart3DVec _ a _) = a
zCoord3D (MkCart3DVec _ _ a) = a

data Cart3DVec' a = MkCart3DVec' {a::a, b::a, c::a}

data Shape = Circle Float |
             Rectangle Float Float

area :: Shape -> Float
area (Circle r)  =  pi * (r^2)
area (Rectangle a b) = a * b

data TrafficLights = Green |
                    Yellow |
                    Red

actionFor :: TrafficLights -> String
actionFor Green = "GO"
actionFor Yellow = "If your close GO else STOP"
actionFor Red = "STOP"
