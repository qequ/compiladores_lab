{-# LANGUAGE GADTs, TypeSynonymInstances, FlexibleInstances #-}
import Control.Applicative ( liftA, liftA2 )

type Var   = String
type Σ     = Var -> Int

{- Dominios semánticos -}
type MInt  = Maybe Int  -- { n | (n = Just m, m ∈ Int)    ∨ (n = Nothing) }
type MBool = Maybe Bool -- { b | (b = Just b', b' ∈ Bool) ∨ (b = Nothing) }

{- Sintaxis -}
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


{- Funciones semánticas -}
class DomSem dom where 
   sem :: Expr dom -> Σ -> dom

instance DomSem MInt where
  sem (CVar a) state = Just (state a)
  sem (CInt a) state = Just a
  sem (Plus a b) state = parse_bin_ops (+) (sem a state) (sem b state)
  sem (Prod a b) state = parse_bin_ops (*) (sem a state) (sem b state)
  sem (Divs a b)  state= if (sem b state) == Just 0 then Nothing else parse_bin_ops (div) (sem a state) (sem b state)
  sem (Op a)  state = parse_bin_ops (-) (Just 0) (sem a state) 


instance DomSem MBool where
  sem (CBool a) state = Just a
  sem (And a b) state= parse_bin_ops (&&) (sem a state) (sem b state)
  sem (Not a) state = parse_un_ops (not) (sem a state) 
  sem (Comp a b) state = parse_bin_ops (==) (sem a state) (sem b state)



{- Funciones auxiliares -}
--(-^-) :: (a -> b -> c) -> (Maybe a -> Maybe b -> Maybe c)
--(-^-) = liftA2

--(-^) :: (a -> b) -> (Maybe a -> Maybe b)
--(-^) = liftA

state :: Σ
state s | s == "z" = 12
        | s == "x" = 0


parse_bin_ops :: (a->b->c) -> Maybe a -> Maybe b -> Maybe c
parse_bin_ops f a b = liftA2 (f) (a) (b)

parse_un_ops :: (a->b) -> Maybe a -> Maybe b 
parse_un_ops f a = liftA (f) (a)


-- ejemplos --
-- sem (Divs (CInt 2) (Plus (CVar "x") (CInt  0))) state == Nothing
-- sem (Divs (Op (Prod (CInt 2) (CInt 2))) (Plus (CInt 1) (CInt 1))) state == Just (-2) 
-- sem (Comp (Divs (Op (Prod (CInt 2) (CInt 2))) (Plus (CInt 1) (CInt 1))) (CVar "x")) state == Just False