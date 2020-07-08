
# Laboratorio Lenguajes y Compiladores #

## Enunciado ##

Extender la implementación de la semántica denotacional para un lenguaje simple
de expresiones aritméticas y booleanas del Lab1 con variables enteras. Además,
utilizar el tipo Maybe de Haskell para capturar el error de la división por cero.

**Tarea**
Extender la sintaxis abstracta con un constructor para las variables
representadas con el tipo String y re-definir la función semántica

                           sem :: Expr dom -> Σ -> dom

que implementa el significado de estas expresiones.

1. Notar que utilizamos como dominio semántico para interpretar las expresiones
   enteras y booleanas, respectivamente, a los tipos "Int ∪ {Nothing}" y
   "Bool ∪ {Nothing}".
2. Notar también que por la definición del tipo Maybe si un entero n ∈ MInt
   entonces debe estar prefijado con el constructor "Just".
3. Revisar las funciones auxiliares al final del archivo. Notar están definidas
   como funciones infijas es recomendable escribir un ejemplo en el interprete.
4. Agregar el constructor para variables.
5. Re-definir la función semántica considerando que ahora necesitamos un
   estado para dar la denotación de las variables.
6. Escribir y ejecutar algunos ejemplos.

## Uso ##

```console
$ ghci Lab2.hs
```
