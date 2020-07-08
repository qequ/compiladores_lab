{-# LANGUAGE GADTs, TypeSynonymInstances, FlexibleInstances #-}
import Control.Applicative 


type MInt = Maybe Int
type MBool = Maybe Bool

data Var = Var String Int
type Σ     = Var -> Int


--dominio semantico
data Expr a where
  CVar :: Var -> Expr MInt

  CInt ::  Int       -> Expr MInt
  CBool :: Bool -> Expr MBool
  -- arithmetic operators
  Plus :: Expr MInt  -> Expr MInt  -> Expr MInt
  Prod :: Expr MInt -> Expr MInt -> Expr MInt
  Divs :: Expr MInt -> Expr MInt -> Expr MInt
  Op :: Expr MInt -> Expr MInt 

  -- logical operators
  And  :: Expr MBool -> Expr MBool -> Expr MBool
  Not  :: Expr MBool -> Expr MBool
  Comp :: Expr MInt -> Expr MInt -> Expr MBool


--funcion semantica
class DomSem dom where 
   sem :: Expr dom -> Σ -> dom

instance DomSem MInt where
  sem (CVar a) get_value = Just (get_value a)
  sem (CInt a) get_value = Just a
  sem (Plus a b) get_value = parse_bin_ops (+) (sem a get_value) (sem b get_value)
  sem (Prod a b) get_value = parse_bin_ops (*) (sem a get_value) (sem b get_value)
  sem (Divs a b)  get_value= if (sem b get_value) == Just 0 then Nothing else parse_bin_ops (div) (sem a get_value) (sem b get_value)
  sem (Op a)  get_value = parse_bin_ops (-) (Just 0) (sem a get_value) 

instance DomSem MBool where
  sem (CBool a) get_value = Just a
  sem (And a b) get_value= parse_bin_ops (&&) (sem a get_value) (sem b get_value)
  sem (Not a) get_value = parse_un_ops (not) (sem a get_value) 
  sem (Comp a b) get_value = parse_bin_ops (==) (sem a get_value) (sem b get_value)


-- auxiliares

get_value:: Σ
get_value (Var s v) = v

parse_bin_ops :: (a->b->c) -> Maybe a -> Maybe b -> Maybe c
parse_bin_ops f a b = liftA2 (f) (a) (b)

parse_un_ops :: (a->b) -> Maybe a -> Maybe b 
parse_un_ops f a = liftA (f) (a)
