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

check_state_trans :: Σ -> Bool
check_state_trans s = (s "z") == 1 && (s "x") == 0

check_omega_ok :: Ω -> Bool
check_omega_ok (Normal s) = check_state_trans s

data Expr a where
  -- # Expresiones enteras
  CInt :: Int       -> Expr MInt                -- n
  V    :: Var       -> Expr MInt                -- v
  Plus :: Expr MInt -> Expr MInt -> Expr MInt   -- e + e'
  Prod :: Expr MInt -> Expr MInt -> Expr MInt
  Divs :: Expr MInt -> Expr MInt -> Expr MInt
  Op :: Expr MInt -> Expr MInt 

  -- # Expresiones booleanas
  CBool :: Bool -> Expr MBool
  Eq   :: Expr MInt  -> Expr MInt -> Expr MBool -- e = e'
  Lt   :: Expr MInt  -> Expr MInt -> Expr MBool -- e < e'
  And  :: Expr MBool -> Expr MBool -> Expr MBool
  Not  :: Expr MBool -> Expr MBool


  -- # Comandos LIS
  Skip :: Expr Ω                                -- skip
  Assign :: Expr MInt -> Expr MInt -> Expr Ω               -- v := e
  Seq :: Expr Ω -> Expr Ω -> Expr Ω                -- c ; c'
  If :: Expr MBool -> Expr Ω -> Expr Ω -> Expr Ω                  -- if b then c else c'
  Newvar :: Var -> Expr MInt -> Expr Ω -> Expr Ω             -- newvar v := e in e'
  While :: Expr MBool -> Expr Ω -> Expr Ω                -- while b do c
  
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
  sem (Prod a b)   = \σ -> ((*)-^-) (sem a σ) (sem b σ)
  sem (Divs a b)   = \σ -> if (sem b σ) == Just 0 then Nothing else ((div)-^-) (sem a σ) (sem b σ)
  sem (Op a)       = \σ -> ((-)-^-) (Just 0) (sem a σ) 
  

instance DomSem MBool where
  -- Completar
  sem (CBool a)  = \_ -> Just a
  sem (And a b)  = \σ -> ((&&)-^-) (sem a σ) (sem b σ)
  sem (Not a)    = \σ -> ((not)-^) (sem a σ) 
  sem (Eq e1 e2) = \σ -> ((==)-^-) (sem e1 σ) (sem e2 σ)
  sem (Lt e1 e2) = \σ -> ((<)-^-) (sem e1 σ) (sem e2 σ)

instance DomSem Ω where
  -- Completar
  sem Skip = \σ -> Normal σ
  sem (Assign (V v) e) = \σ -> (update_var σ v (sem e σ))
  sem (Seq c1 c2) = \σ -> (*.) (sem c2) (sem c1 σ) 
  sem (If b c0 c1) = \σ -> if (sem b σ) == (Just True) then (sem c0 σ) else (sem c1 σ)
--  sem (Newvar v e c) = \σ -> (†.) (\s -> update s v (get_value_var σ v)) (sem c (update σ v (unpack_mint (sem e σ))))
  sem (Newvar v e c) = \σ -> (†.) (\s -> update s v (get_value_var σ v)) (eval_newvar c σ v e)


-- funciones auxiliares de ecuaciones semánticas
get_value_var :: Σ -> Var -> Int
get_value_var s v = (s v)


unpack_mint :: Maybe Int -> Int
unpack_mint (Just n) = n

eval_newvar ::  Expr Ω -> Σ -> Var -> Expr MInt -> Ω
eval_newvar c σ v e = if (sem e σ) == Nothing then Abort σ else (sem c (update σ v (unpack_mint (sem e σ))))


update_var :: Σ -> Var -> Maybe Int  -> Ω
update_var σ _ Nothing = Abort σ
update_var σ v (Just n) = Normal (update σ v n)

--update :: Σ -> Var -> Int -> Σ


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



-- state para probar
state :: Σ
state s | s == "z" = 12
        | s == "x" = 0



-- ejemplos para chequear las semánticas del lab 2
-- sem (Divs (CInt 2) (Plus (V "x") (CInt  0))) state == Nothing
-- sem (Divs (Op (Prod (CInt 2) (CInt 2))) (Plus (CInt 1) (CInt 1))) state == Just (-2)
-- sem (Eq (Divs (Op (Prod (CInt 2) (CInt 2))) (Plus (CInt 1) (CInt 1))) (V "x")) state == Just False
-- sem (Eq (V "x") (V "z")) state == Just False

-- ejemplos LIS
--  sem (Assign (V "x") (CInt 3)) state 
--  sem (Seq (Assign (V "z") (CInt 3)) (Assign (V "x") (CInt 3))) state
--  sem (If (Eq (V "x") (V "z")) (Assign (V "x") (CInt 3)) (Assign (V "z") (CInt 3))) state
--  sem (Newvar "x" (CInt 1) (Assign (V "z") (V "x"))) state

