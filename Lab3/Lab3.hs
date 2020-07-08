{-# LANGUAGE GADTs, TypeSynonymInstances, FlexibleInstances #-}
import Control.Applicative ( liftA, liftA2 )

--         ∞
-- fix f = ⊔ fⁱ ⊥
--        i=0
fix :: (a -> a) -> a
fix f = f (fix f)

type Var = String
type Σ   = Var -> Int

{- Dominios semánticos -}
type MInt  = Maybe Int  -- { n | (n = Just m, m ∈ Int)    ∨ (n = Nothing) }
type MBool = Maybe Bool -- { b | (b = Just b', b' ∈ Bool) ∨ (b = Nothing) }

{- Ω ≈ (Σ' + Z × Ω + Z → Ω)⊥ -}
data Ω = Normal Σ | Abort Σ | Out (Int, Ω) | In (Int -> Ω)
{- Notar:
   * Normal : Σ → Ω
   * Abort  : Σ → Ω
   * Out    : (Z, Ω) → Ω
   * In     : (Z → Ω) → Ω
-}

update :: Σ -> Var -> Int -> Σ
update σ v n v' = if v == v' then n else σ v'

data Expr a where
  -- # Expresiones enteras
  CInt :: Int       -> Expr MInt                -- n
  V    :: Var       -> Expr MInt                -- v
  Plus :: Expr MInt -> Expr MInt -> Expr MInt   -- e + e'
  -- # Expresiones booleanas
  Eq   :: Expr MInt  -> Expr MInt -> Expr MBool -- e = e'
  Lt   :: Expr MInt  -> Expr MInt -> Expr MBool -- e < e'
  -- # Comandos LIS
  Skip :: Expr Ω                                -- skip
  -- v := e
  -- c ; c'
  -- if b then c else c'
  -- newvar v := e in e'
  -- while b do c
  
  -- # Comandos Fallas
  -- fail
  -- catch c with c'

  -- # Comandos IO
  -- !e
  -- ?v

class DomSem dom where 
  sem :: Expr dom -> Σ -> dom

instance DomSem MInt where
  -- Completar
  sem (CInt a)     = \_ -> Just a
  sem (V v)        = \σ -> Just $ σ v
  sem (Plus e1 e2) = \σ -> ((+)-^-) (sem e1 σ) (sem e2 σ)

instance DomSem MBool where
  -- Completar
  sem (Eq e1 e2) = \σ -> ((==)-^-) (sem e1 σ) (sem e2 σ)
  sem (Lt e1 e2) = \σ -> ((<)-^-) (sem e1 σ) (sem e2 σ)

instance DomSem Ω where
  -- Completar
  sem Skip = \σ -> Normal σ

(>>==) :: (Maybe a, Σ) -> (a -> Ω) -> Ω
(>>==) (Nothing, σ) _ = Abort σ
(>>==) (Just n, _)  f = f n

(*.) :: (Σ -> Ω) -> Ω -> Ω
(*.) f (Normal σ)  = f σ
(*.) _ (Abort σ)   = Abort σ
(*.) f (Out (n,ω)) = Out (n, f *. ω)
(*.) f (In g)      = In ((f *.) . g)

(†.) :: (Σ -> Σ) -> Ω -> Ω
(†.) f (Normal σ)  = Normal $ f σ
(†.) f (Abort σ)   = Abort $ f σ
(†.) f (Out (n,ω)) = Out (n, f †. ω)
(†.) f (In g)      = In ((f †.) . g)

(+.) :: (Σ -> Ω) -> Ω -> Ω
(+.) _ (Normal σ)  = Normal σ
(+.) f (Abort σ)   = f σ
(+.) f (Out (n,ω)) = Out (n, f +. ω)
(+.) f (In g)      = In ((f +.) . g)

{- Funciones de evaluación de dom -}

class Eval dom where 
  eval :: Expr dom -> Σ -> IO ()

instance Eval MInt where
  eval e = putStrLn . show . sem e

instance Eval MBool where
  eval e = putStrLn . show . sem e

instance Eval Ω where
  eval e = unrollOmega . sem e
    where unrollOmega :: Ω -> IO ()
          unrollOmega (Normal _)   = return ()
          unrollOmega (Abort _)    = putStrLn "Abort"
          unrollOmega (Out (n, ω)) = putStrLn (show n) >> unrollOmega ω
          unrollOmega (In f)       = getLine >>= unrollOmega . f . read

{- Funciones auxiliares -}
(-^-) :: (a -> b -> c) -> (Maybe a -> Maybe b -> Maybe c)
(-^-) = liftA2

(-^) :: (a -> b) -> (Maybe a -> Maybe b)
(-^) = liftA
