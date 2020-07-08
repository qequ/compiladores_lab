import Control.Applicative 

f1:: Int -> Int -> Int 
f1 x y = 2*x+y  

type MInt  = Maybe Int  -- { n | (n = Just m, m ∈ Int)    ∨ (n = Nothing) }

my_add_M :: MInt -> MInt -> MInt
my_add_M Nothing x = Nothing
my_add_M x Nothing = Nothing
my_add_M (Just a) (Just b) = Just (a+b)


data Shape = Circle Float Float Float | Rectangle Float Float Float Float
--data Var = Var String Int 

main = do
    print(fmap (&& True) (Just True))


type Σ  = Var -> Int

test_if :: Int -> Int -> Int
test_if a b = if b == 0 then a * b else 0


parse_bin_ops :: (a->b->c) -> Maybe a -> Maybe b -> Maybe c
parse_bin_ops f a b = liftA2 (f) (a) (b)

parse_un_ops :: (a->b) -> Maybe a -> Maybe b 
parse_un_ops f a = liftA (f) (a)

(-^) :: (a -> b) -> (Maybe a -> Maybe b)
(-^) = liftA

f :: Int -> Int
f a = 2 * a


type Var = String

state :: Σ
state "x" = 23
state a = 0

--get_value:: Σ
--get_value (Var s v) = v

