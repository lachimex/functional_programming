import Data.List
sortDesc :: Ord a => [a] -> [a]
sortDesc = reverse.sort

are2FunsEqAt :: Eq a => (t -> a) -> (t -> a) -> [t] -> Bool
are2FunsEqAt f g =  all (\x -> f x == g x)