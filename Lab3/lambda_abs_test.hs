lam :: Int -> Int -> Int
lam x = \y -> x + y


lam2 :: Int -> Int
lam2 x = (\y -> (\z -> x + y + z) ) 2 3 