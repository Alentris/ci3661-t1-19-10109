-- =====================================================================
--  CI-3661 — Tarea 1 (Haskell) — Septiembre–Diciembre 2026
--
--  Nombre: Alejandro De Vincenzo
--  Carnet: 19-10109
--
--  Declaración de uso de herramientas de IA (obligatoria, ver enunciado): Gemini
--  Declaro haber usado Gemini para consultar errores y recordar cómo usar funciones
--  básicas del propio Haskell.
--
-- =====================================================================
--  No cambies el nombre del módulo ni la lista de exportación, ni las
--  firmas de las funciones. Puedes agregar todas las funciones auxiliares
--  que quieras. El único import permitido es Data.Char.
-- =====================================================================
module Tarea1
  ( esPalindromo
  , mayorYMenor
  , leerHora
  , duracion
  , promedioSobreUmbral
  , movimientosCaballo
  , enDosSaltos
  , miMap
  , miFilter
  , miFoldl
  ) where

import Data.Char (isAlphaNum, isDigit, toLower, digitToInt)

-- =====================================================================
-- Parte A: recursión explícita.
-- Prohibido: map, filter, foldr, foldl, listas por comprensión, reverse,
-- maximum, minimum, all, any y cualquier otra función de orden superior.
-- =====================================================================

-- A1
esPalindromo :: String -> Bool
esPalindromo a
  | alfaNumerico (minuscula a) == inverso (alfaNumerico (minuscula a)) = True
  | otherwise = False

inverso :: String -> String
inverso "" = ""
inverso (x:xs) = inverso xs ++ [x]

minuscula :: String -> String
minuscula "" = ""
minuscula (x:xs) = [toLower x] ++ minuscula xs

alfaNumerico :: String -> String
alfaNumerico "" = ""
alfaNumerico (x:xs)
  | isAlphaNum x = [x] ++ alfaNumerico xs
  | otherwise = alfaNumerico xs

-- A2
-- Explica aquí, en dos o tres líneas, por qué tu solución recorre la
-- lista una sola vez: porque en cada paso recursivo se hace una sola llamada
-- a la función para la cola de la lista (xs), actualizando el máximo y el
-- mínimo simultáneamente de manera lineal.

mayorYMenor :: Ord a => [a] -> Maybe (a, a)
mayorYMenor [] = Nothing
mayorYMenor [x] = Just (x, x)
mayorYMenor (x:xs) = a (mayorYMenor xs)
  where
    a Nothing = Just (x, x)
    a (Just (maxCola, minCola)) = Just (maxNuevo, minNuevo)
      where
        maxNuevo
          | x > maxCola = x 
          | otherwise   = maxCola
        minNuevo
          | x < minCola = x 
          | otherwise   = minCola

-- A3
leerHora :: String -> Either String (Int, Int)
leerHora [h1, h2, ':', m1, m2]
  | not (isDigit h1 && isDigit h2 && isDigit m1 && isDigit m2) = Left "formato inválido"
  | hora > 23  = Left "hora fuera de rango"
  | minuto > 59  = Left "minutos fuera de rango"
  | otherwise = Right (hora, minuto)
  where
    hora = digitToInt h1 * 10 + digitToInt h2
    minuto = digitToInt m1 * 10 + digitToInt m2
leerHora _ = Left "formato inválido"

duracion :: String -> String -> Either String Int
duracion hora1 hora2 = evaluarHora1 (leerHora hora1)
  where
    evaluarHora1 (Left error1) = Left error1
    evaluarHora1 (Right t1)  = evaluarHora2 (leerHora hora2) t1

    evaluarHora2 (Left error2) _  = Left error2
    evaluarHora2 (Right t2)  (h1, m1) = calcularDiferencia (h1, m1) t2

    calcularDiferencia (h1, m1) (h2, m2) =
        let totalMinutos1 = h1 * 60 + m1
            totalMinutos2 = h2 * 60 + m2
            diferencia = totalMinutos2 - totalMinutos1
        in if diferencia >= 0 
           then Right diferencia 
           else Right (diferencia + 24 * 60)

-- =====================================================================
-- Parte B: orden superior.
-- Prohibido: recursión explícita (ninguna función de esta parte, ni sus
-- auxiliares, puede llamarse a sí misma).
-- =====================================================================

-- B1
promedioSobreUmbral :: Double -> [Double] -> Maybe Double
promedioSobreUmbral = undefined

-- B2
movimientosCaballo :: (Int, Int) -> [(Int, Int)]
movimientosCaballo = undefined

enDosSaltos :: (Int, Int) -> Int
enDosSaltos = undefined

-- B3
miMap :: (a -> b) -> [a] -> [b]
miMap = undefined

miFilter :: (a -> Bool) -> [a] -> [a]
miFilter = undefined

miFoldl :: (b -> a -> b) -> b -> [a] -> b
miFoldl = undefined