sqr x = x^2

funcFactory n = case n of
 1 -> id
 2 -> sqr
 3 -> (^3)
 4 -> \x -> x^4
 5 -> intFunc
 _ -> const n
 where
   intFunc x = x^5

expApproxUpTo n = case n of
    0 -> \x -> 1
    1 -> \x -> x + 1
    2 -> \x -> x/2 + expApproxUpTo 1 x
    3 -> \x -> x/6 + expApproxUpTo 2 x
    4 -> \x -> x/24 + expApproxUpTo 3 x
    5 -> \x -> x/120 + expApproxUpTo 4 x