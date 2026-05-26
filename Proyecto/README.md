
# Codificación de Huffman


## Objetivo
Reducir la longitud de la representación binaria de una cadena mediante la codificación de Huffman.

## Funcionamiento
- Calculo de las frecuencias de los caracteres de la cadena
- Construcción del árbol de Huffman a partir de las hojas
- Códificación la cadena recorriendo el árbol (y se puede decodificar usando el mismo árbol).

Las funciones principales son:

```
codificacionHuffman
decodificacionHuffman
arbolHuffman
frecDescP
```
Considere que todos reciben como entrada una cadena. 

Las funciones auxiliares (por ejemplo: cálculo de frecuencias, construcción de hojas y árbol, funciones de apoyo para  codificar o decodificar) estan en `Auxiliar.hs`.

## Ejecución
- Desde la carpeta Proyecto ejecutar en terminal:
```bash
ghci Proyecto.hs
```


## Tiempo requerido
12 horas 
