{-# LANGUAGE GADTs, TypeSynonymInstances, FlexibleInstances #-}
import Control.Applicative ( liftA, liftA2 )

--dominio semantico
type MInt = Maybe Int
type MBool = Maybe Bool

data Expr a where
  CInt :: Int       -> Expr MInt
  CBool :: Bool -> Expr MBool
  -- arithmetic operators
  Plus :: Expr MInt  -> Expr MInt  -> Expr MInt
  Prod :: Expr MInt -> Expr MInt -> Expr MInt
  Divs :: Expr MInt -> Expr MInt -> Expr MInt
  Op :: Expr MInt -> Expr MInt 

  -- logical operators
  And  :: Expr MBool -> Expr MBool -> Expr MBool
  Not  :: Expr MBool -> Expr MBool
  Eq :: Expr MInt -> Expr MInt -> Expr MBool


--funcion semantica
class DomSem dom where 
   sem :: Expr dom -> dom

instance DomSem MInt where
   sem (CInt a) = (Just a)
   sem (Plus a b) = ((+)-^-) (sem a) (sem b)
   sem (Prod a b) = ((*)-^-) (sem a)  (sem b)
   sem (Divs a b) = if (sem b) == Just 0 then Nothing else ((div)-^-) (sem a) (sem b)
   sem (Op a) = ((-)-^-) (Just 0) (sem a)

instance DomSem MBool where
   sem (CBool a) = (Just a)
   sem (And a b) = ((&&)-^-) (sem a) (sem b)
   sem (Not a) = ((not)-^) (sem a)
   sem (Eq a b) = ((==)-^-) (sem a) (sem b)


(-^-) :: (a -> b -> c) -> (Maybe a -> Maybe b -> Maybe c)
(-^-) = liftA2

(-^) :: (a -> b) -> (Maybe a -> Maybe b)
(-^) = liftA

-- ejemplos
-- sem (Plus (CInt 2) (Prod (CInt 3)(CInt 3))) == Just 11
-- sem (Plus (Divs (CInt 8)(CInt 2)) (Op (CInt 2))) == Just 2
-- sem (And (CBool True)(Not (CBool False))) == Just True
-- sem (Eq (CInt 4)(Prod (CInt 2)(CInt 2))) == Just True
-- sem (Divs (CInt 2) (CInt 0)) == Nothing
-- sem (Eq (CInt 2) (Divs (CInt 2)(CInt 0))) == Nothing