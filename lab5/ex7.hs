newtype Box a = MkBox a deriving Show

instance Functor Box where
  fmap f (MkBox x) = MkBox (f x)

instance Applicative Box where
  pure = MkBox
  (MkBox f) <*> (MkBox w) = MkBox (f w)

newtype MyTriple a = MyTriple (a,a,a) deriving Show

instance Functor MyTriple where
    fmap f (MyTriple (a, b, c)) = MyTriple (f a, f b, f c)

instance Applicative MyTriple where
    pure x = MyTriple (x, x, x)
    (MyTriple (fa, fb, fc)) <*> (MyTriple (a, b, c)) = MyTriple (fa a, fb b, fc c)