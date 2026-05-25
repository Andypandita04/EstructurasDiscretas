module Proyecto where
import Auxiliar

{-
    Funcion para ordenar las frecuencias (una lista de duplas (Char,Int)) 
    De mayor a menor  
-}
frecuenciadDescescientes :: [(Char,Int)] -> [(Char,Int)]
frecuenciadDescescientes [] = []
frecuenciadDescescientes (x:xs) = insertarDesc x (frecuenciadDescescientes xs)


{-
    Funcion para obtener el arbol de huffman de una cadena dada
    - primero sacamos las frecuencias de los caracteres de la cadena dada
    - De cada frecuecia se crea una hoja de huffman y se crea una lista
    - De la lista se crea el arbol de huffman, 
-}
arbolHuffman :: String -> Arbol
arbolHuffman [] = Vacio
arbolHuffman s = crearArbolHuffman  (hojasHuffman (  frecString s))


{-
    Funcion para obtener la codificacion de Huffman
    - obtenemos el arbol de huffman
    - por cada caracter se recorre el arbol para obtener su codificacion
    - se van concatenando las codificacion de cada caracter 
-}
codificacionHuffman :: String ->  String
codificacionHuffman [] = []
codificacionHuffman (x:xs) = codificarCadena (x:xs) (arbolHuffman (x:xs))

{-
    Funcion para obtener la decodificacion de Huffman
-}
decodificacionHuffman :: String -> Arbol -> String
decodificacionHuffman [] _ = []
decodificacionHuffman s Vacio = []  
decodificacionHuffman s arbol = decodificarCadena s arbol



