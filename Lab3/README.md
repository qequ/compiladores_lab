# Laboratorio Lenguajes y Compiladores #

## Enunciado ##

Extender el lenguaje definido en los laboratorios 1 y 2 para implementar
el lenguaje imperativo simple + fallas + input-output (LIS+Fallas+IO).

**Tarea**
Extender la sintaxis abstracta con los constructores correspondientes y 
re-definir la función semántica

                           sem :: Expr dom → Σ → dom

que implementa el significado de estas expresiones; implementar las instancias
de la clase DomSem para MInt, MBool y Ω.

1.   Agregar los constructores sintácticos correspondientes al lenguaje
     imperativo simple: newvar, asignación, while, sentencia condicional y
     composición.
1.5. Implementar su función semántica. Al no tener fallas e input-output nuestro
     dominio alcanza con Σ⊥, sin embargo no podemos representar (razonablemente)
     el error producido por una división por cero. Por ejemplo, si consideramos
     el programa "x := 3 / 0", entonces tendríamos dos opciones a priori; la
     primera es que la implementación propague Nothing (pensar si esto es
     posible dada nuestra definición de Ω), otra posibilidad es devolver "Abort
     σ" para algún σ (opción sugerida). Para esta última es útil definirse una
     función que transforme un posible error representado con Nothing a un error
     representado con Abort; meditar sobre la función ya definida (>>==). En
     este caso notar que estaríamos utilizando Σ'⊥. Otra opción más (para
     pensar) es utilizar ⊥ para representar Nothing en Σ⊥; ¿quién representa a ⊥
     en la implementación? ¿Cuál es una posible definición de bot :: Ω?

2.   Agregar los constructores sintácticos para extender el lenguaje con fallas.
2.5. Implementar su función semántica teniendo en cuenta que el error de
     división por cero queremos representarlo con abort.

3.   Agregar los constructores sintácticos para extender el lenguaje con
     input-output
3.5. Implementar su función semántica.

## Ejemplos ##

Asumiendo algunos nombres de constructores, particularmente SOut (!) y
SIn (?).

**Ejemplo 1**
while x < 10 do
  !x ;
  x := x + 1
od

ej1 :: Expr Ω
ej1 = While (Lt (Var "x") (Const 10)) $
            Seq (SOut $ Var "x")
                (Assign "x" (Plus (Var "x") (Const 1)))

eval_ej1 :: IO ()
eval_ej1 = eval ej1 (\_ -> 0)

**Ejemplo 2**
while y < 10 do
  ?x ;
  !x ;
  !y ;
  y := y + 1
od

ej2 :: Expr Ω
ej2 = While (Lt (Var "y") (Const 10)) $
            Seq (Seq (Seq (SIn "x")
                          (SOut $ Var "x")
                     )
                     (SOut $ Var "y")
                )
                (Assign "y" (Plus (Var "y") (Const 1)))

eval_ej2 :: IO ()
eval_ej2 = eval ej2 (\_ -> 0)

**Ejemplo 3**
?x ;
newvar x := 10 in !x end ;
!x

ej3 :: Expr Ω
ej3 = Seq (Seq (SIn "x")
               (Newvar "x" (Const 10)
                       (SOut $ Var "x")
               )
          )
          (SOut $ Var "x")

eval_ej3 :: IO ()
eval_ej3 = eval ej3 (\_ -> 0)

## Uso ##

```console
$ ghci Lab3.hs
*Main> eval_ej1
0
1
2
3
4
5
6
7
8
9
*Main>
```
