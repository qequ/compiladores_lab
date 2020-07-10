{-# LANGUAGE GADTs #-}

--dominio semantico
data Expr a where
  CInt :: Int       -> Expr Int
  CBool :: Bool -> Expr Bool
  -- arithmetic operators
  Plus :: Expr Int  -> Expr Int  -> Expr Int
  Prod :: Expr Int -> Expr Int -> Expr Int
  Divs :: Expr Int -> Expr Int -> Expr Int
  Op :: Expr Int -> Expr Int 

  -- logical operators
  And  :: Expr Bool -> Expr Bool -> Expr Bool
  Not  :: Expr Bool -> Expr Bool
  Eq :: Expr Int -> Expr Int -> Expr Bool


--funcion semantica
class DomSem dom where 
   sem :: Expr dom -> dom

instance DomSem Int where
   sem (CInt a) = a
   sem (Plus a b) = (sem a) + (sem b)
   sem (Prod a b) = (sem a) * (sem b)
   sem (Divs a b) = (sem a) `div` (sem b)
   sem (Op a) = -(sem a)

instance DomSem Bool where
   sem (CBool a) = a
   sem (And a b) = (sem a) && (sem b)
   sem (Not a) = not (sem a)
   sem (Eq a b) = (sem a) == (sem b)


-- ejemplos
-- sem (Plus (CInt 2) (Prod (CInt 3)(CInt 3))) = 11
-- sem (Plus (Divs (CInt 8)(CInt 2)) (Op (CInt 2))) = 2
-- sem (And (CBool True)(Not (CBool False))) = True
-- sem (Eq (CInt 4)(Prod (CInt 2)(CInt 2))) = True