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
sinRepetidos :: (Eq a) => [a] -> [a]
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
    De menor a mayor
-}
insertar :: (Char,Int) -> [(Char,Int)] -> [(Char,Int)]
insertar x [] = [x]
insertar x (y:ys)
    |  (snd x) <= (snd y)  = x : y : ys     -- snd el segundo elemento de la dupla
    | otherwise            = y : (insertar x ys)

{-
    Funcion para ordenar las frecuencias (una lista de duplas (Char,Int)) 
    De menor a mayor 
-}
frecOrd :: [(Char,Int)] -> [(Char,Int)]
frecOrd [] = []
frecOrd (x:xs) = insertar x (frecOrd xs)

{-
    Funcion para obtener todas las frecuencias de los caracteres de una cadena ordenadas de mayor a menor
-}
frecString :: String -> [(Char,Int)]
frecString s = frecOrd (frecuencias s)


--
--
--
--
--
--
--
--

data Arbol
    = Vacio
    | Hoja Char Int                 --- Caracter y frecuencia 
    | Nodo Int Arbol Arbol         --- Peso de la hoja = la suma de la freceuencia de los caracteres del subarbol
        deriving (Show, Eq)

{-
    Funcion para crear hoja de la frecuencia de un caracter (Char,Int)
-}
crearHoja :: (Char,Int) -> Arbol
crearHoja (c,f) = Hoja c f  

{- 
    Funcion para crear las hojas de huffman a partir de una lista de frecuencias (Char,Int)
-}
hojasHuffman :: [(Char,Int)] -> [Arbol]
hojasHuffman [] = []
hojasHuffman (x:xs) = (crearHoja x) : hojasHuffman xs

{-
    Funcion para obtener la frecuencia o peso de un nodo
-}
frecuenciaNodo  :: Arbol -> Int
frecuenciaNodo Vacio = 0    
frecuenciaNodo (Hoja _ f) = f
frecuenciaNodo (Nodo f _ _) = f

{-
Funcion para crear un arbol de huffman apartir de otros arboles de huffman 
-}
unirArboles :: Arbol -> Arbol -> Arbol
unirArboles a b = Nodo (frecuenciaNodo a + frecuenciaNodo b) a b


{-
    Funcion para insertar un arbol de huffman en una lista de arboles de huffman ordenada por frecuencia
-}
insertarArbol :: Arbol -> [Arbol] -> [Arbol]
insertarArbol a [] = [a]
insertarArbol a (x:xs)
    | frecuenciaNodo a <= frecuenciaNodo x = a : x : xs
    | otherwise = x : insertarArbol a xs

{-
    Dada una lista de arboles de huffman (idelamente las hojas de huffman, ie, las frecuencias de los caracteres)
    ,regresa la construccion del arbol de huffman
-}
crearArbolHuffman :: [Arbol] -> Arbol
crearArbolHuffman [] = Vacio
crearArbolHuffman [x] = x
crearArbolHuffman (x:y:xs) = crearArbolHuffman (insertarArbol (unirArboles x y) xs)

