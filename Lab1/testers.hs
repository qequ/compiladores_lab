f :: Int -> Int
f x = 
    if is_odd (x) then  2 * x
    else (x + 1) * 2


g :: [Int] -> Bool
g (x:xs) = True
g [] = False

is_odd :: Int -> Bool
is_odd x = (x `mod` 2) == 0

