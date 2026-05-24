module Proyecto where
import Auxiliar

{-
    Funcion para obtener el arbol de huffman de una cadena dada
    - primero sacamos las frecuencias de los caracteres de la cadena dada
    - De cada frecuecia se crea una hoja de huffman y se crea una lista
    - De la lista se crea el arbol de huffman, 
      uniendo los dos arboles con menor frecuencia hasta que solo quede un arbol

-}
arbolHuffman :: String -> Arbol
arbolHuffman [] = Vacio
arbolHuffman s = crearArbolHuffman  (hojasHuffman (  frecString s))