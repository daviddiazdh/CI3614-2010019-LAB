-------------------------------------------------------------------------------
-- Tarea 1 - Laboratorio de Lenguajes de Programación I (CI-3661)
-- Nombre del estudiante: David Alejandro Díaz Herrera
-- Carnet: 20-10019
-------------------------------------------------------------------------------

module Tarea1 where
import Data.Char (toUpper)

-------------------------------------------------------------------------------
-- Problema 1: Palíndromo
-------------------------------------------------------------------------------

-- Implementa una función que determine si una cadena es un palíndromo
-- (se lee igual al derecho y al revés). La función debe devolver un simple
-- valor booleano (True o False). Implementa esto usando recursión explícita.
esPalindromo :: String -> Bool
esPalindromo [] = True
esPalindromo [_] = True
esPalindromo (x:xs) = last xs == x && esPalindromo (init xs)

-------------------------------------------------------------------------------
-- Problema 2: Producto de Elementos Pares en una Lista
-------------------------------------------------------------------------------

-- Implementa una función puramente recursiva (sin usar map, fold o filter
-- inicialmente) que calcule el producto de todos los elementos en una lista
-- de números enteros, pero solo incluyendo aquellos elementos que son pares.
-- La función debe devolver 1 si la lista está vacía o no contiene números pares.
productoParesRec :: [Integer] -> Integer
productoParesRec [] = 1
productoParesRec (x:xs) = if (mod x 2 == 0) then x * productoParesRec(xs) else productoParesRec(xs)

-------------------------------------------------------------------------------
-- Problema 3: Parseo Condicional con Either
-------------------------------------------------------------------------------

-- Implementa una función que tome una lista de cadenas. Para cada cadena,
-- intenta convertirla a un número entero (Int). Si la conversión es exitosa,
-- devuelve Right Int. Si la conversión falla (porque la cadena no es puramente
-- numérica), devuelve Left String con la cadena original transformada a mayúsculas.
-- Nota: Necesitarás funciones auxiliares para el parseo seguro y la conversión a mayúsculas.

-- Tengo entendido que se podía utilizar readMaybe, pero no fue incluido en los imports de Test.hs,
-- por eso preferí usar pattern matching

parsearCondicional :: [String] -> [Either String Int]
parsearCondicional = map parseOne
    where
        parseOne x = case reads x of
            [(n, "")] -> Right n
            _         -> Left (map toUpper x)

-------------------------------------------------------------------------------
-- Problema 4: Suma Acumulada Condicional
-------------------------------------------------------------------------------

-- Implementa una función que reciba una lista de números de punto flotante y un
-- umbral (Float). La función debe filtrar la lista, manteniendo solo los números
-- que son mayores que el umbral, y luego calcular la suma de los números
-- filtrados utilizando una operación de plegado (fold).
sumaAcumuladaCondicional :: Float -> [Float] -> Float
sumaAcumuladaCondicional x y = foldl(+) 0 (filter (\i -> i > x) y) 

-------------------------------------------------------------------------------
-- Problema 5: Generación de Coordenadas Impares
-------------------------------------------------------------------------------

-- Implementa una función que tome un número entero positivo N y genere una lista
-- de pares de coordenadas (x, y) donde x e y están en el rango [1, N] y la
-- suma de x e y (x+y) es un número impar. Utiliza Listas por Comprensión para
-- la implementación.
coordenadasImpares :: Int -> [(Int, Int)]
coordenadasImpares n = [(x, y) | x <- [1..n], y <-[1..n], odd(x + y)]

-------------------------------------------------------------------------------
-- Problema 6: Descomposición Segura de Lista
-------------------------------------------------------------------------------

-- Implementa una función recursiva que divida una lista en su cabeza (head) y
-- el resto (tail), pero lo haga de manera segura utilizando el tipo Maybe.
-- Si la lista está vacía, devuelve Nothing. Si la lista no está vacía,
-- devuelve Just con una tupla que contiene el primer elemento y el resto de la lista.
descomponerListaSegura :: [a] -> Maybe (a, [a])
descomponerListaSegura [] = Nothing
descomponerListaSegura (x:xs) = Just (x, xs)

-------------------------------------------------------------------------------
