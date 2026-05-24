module Auxiliar where


{-
Funcion que indica si un elemento se encuentra en la lista 
-}
contiene :: (Eq a) =>  a -> [a] -> Bool
contiene a [] = False
contiene a (x:xs) = x == a || contiene a xs

{-
Funcion que dada una lista, te regresa la lista sin elementos repetidos
-}
sinRepetidos :: [a] -> [a]
sinRepetidos [] = []
sinRepetidos (x:xs) 
    | (contiene x xs) = sinRepetidos xs
    | otherwise = x:(sinRepetidos xs)
 

{-
Función que dado un elemento 'a' y un una lista 'b', regresa cuantas veces aparece 'a' en 'b' 
-}
contar :: Eq a => a -> [a] -> Int
contar _ [] = 0
contar a (x:xs)
    | a == x    = 1 + (contar a xs)
    | otherwise = (contar a xs)


{-
    Funcion para obtener las frecuencias de todos los caracteres de una cadena 
-}
frecuencias :: String -> [(Char,Int)]
frecuencias [] = []
frecuencias s  = [(y ,  contar y s  ) | y <- sinRepetidos s]


{-
    Funcion para insertar un elemento de forma ordenada una lista de duplas (Char,Int)
-}
insertar :: (Char,Int) -> [(Char,Int)] -> [(Char,Int)]
insertar x [] = [x]
insertar x (y:ys)
    |  (snd x) >= (snd y)  = x : y : ys     -- snd el segundo elemento de la dupla
    | otherwise            = y : (insertar x ys)

{-
    Funcion para ordenar las frecuencias (una lista de duplas (Char,Int)) de mayor a menor
-}
frecDesc :: [(Char,Int)] -> [(Char,Int)]
frecDesc [] = []
frecDesc (x:xs) = insertar x (frecDesc xs)

