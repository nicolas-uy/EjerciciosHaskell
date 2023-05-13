module Main where
import Data.Char

-- Ejercicio1 - Definir en Haskell la function “mitad” tal que mitad x retorna la m itad x. X es tipo Double.
mitad :: Double -> Double
mitad x = x / 2

-- Ejercicio2 - Definir la function sumar tal que sumar I es la suma de los elemtos de la lista I. 
sumar :: [Int]-> Int
sumar[] = 0
sumar (x:xs) = x + sumar(xs)

-- Ejercicio3 - función concat tal que concat l es la concatenación de las listas de l
unir :: [[a]] -> [a]
unir [] = []
unir (x:xs) = x ++ concat xs

-- Ejercicio4 - Funcion esPositivo
esPositivo :: (Ord a, Num a) => a -> Bool
esPositivo x = x > 0

-- Ejercicio5 - Definir la función “producto” tal que producto l es el producto de los elementos de l.
producto :: [Int] -> Int
producto [] = 1
producto (x:xs) = x * producto xs

-- Ejercicio6 - Definir la función “or” tal que or l se verifica si algún elemento de l es verdadero.
or' :: [Bool] -> Bool
or' [] = False
or' (x:xs) = x || or' xs

-- Ejercicio7 - Invertir una lista: El operador Ord me sirve para indicar que representa a cualquier tipo de dato.
invertir :: Ord a => [a] -> [a]
invertir [] = []
invertir (x:xs) = (invertir xs) ++ [x]

-- Ejercicio8 - Función comparar si 2 listas son iguales:
sonIguales :: Eq a => [a] -> [a] -> Bool
sonIguales [] [] = True
sonIguales [] _ = False
sonIguales _ [] = False
sonIguales (x:xs) (y:ys) = x == y && sonIguales xs ys

-- Ejercicio9 - Función siguiente número.
siguiente :: Int -> Int
siguiente numero = numero + 1

-- Ejercicio10 - 
buscarLetra :: [String] -> String -> Int
buscarLetra [] l = error "No se encontro letra"
buscarLetra (x:xs) l | x /= l = 1 + buscarLetra xs l | otherwise = 0
listaDeLetras :: [String]
listaDeLetras = [pure c | c <- ['a'..'z']]

--Ejercicio11 - Función que eleva número al cuadrado.
numCuadrado = map (\ x -> x * x)

--Ejercicio12 - Función que filtra valores pares de una lista mayores a 10.
filtrarPar :: [Int] -> [Int]
filtrarPar = filter (\ x -> even x && x > 10)

--Ejercicio13 - Funcion que suma los cuadrados de una lista.
sumaCuadrados :: [Int] -> Int
sumaCuadrados = sum . map (^2)

--Ejercicio14 - Funcion que verifica si esta dentro del rango de edad de 71 a 79.
rangoEdad :: Int -> String
rangoEdad edad = if edad >= 71 && edad <= 79 then "Está dentro del rango de edad." else "No está dentro del rango de edad."

--Ejercicio15 - Escribir una expresión lambda que dada una lista de edades y una cantidad n, devuelve los n elementos que están dentro de un rango etario por ejemplo (71 - 79).
rangoEdadLista :: Int -> Bool
rangoEdadLista edades = edades >= 71 && edades <= 79
filtrarRangoEtario :: [Int] -> Int -> [Int]
filtrarRangoEtario edades n = take n (filter rangoEdadLista edades)

--Ejercicio16
filtrarMultiplicadosPor3MayoresA5 :: [Int] -> [Int]
filtrarMultiplicadosPor3MayoresA5 = filter (\x -> 3 * x > 5)

--Ejercicio18 - Número primo.
esPrimo :: Int -> Bool
esPrimo n = n > 1 && all (\x -> n `mod` x /= 0) [2..floor (sqrt (fromIntegral n))]

-- Ejercicio 19
ultimaCifra :: Integer -> Integer
ultimaCifra x = x `mod` 10

-- Ejercicio 20
rotar :: [a] -> [a]
rotar [] = []
rotar (x:xs) = xs ++ [x]

-- Ejercicio21 - 
numeroDeDigitos :: Int -> Int
numeroDeDigitos x = length (show x)

-- Ejercicio22 - Función de número capicua.
capicua :: Int -> Bool
capicua n = reverse (show n) == show n

-- Ejercicio23 - Función HEAD que retorna el primer elemento de la lista.
headLista :: [a] -> a
headLista [] = error "Lista vacía"
headLista (x:_) = x


-- Ejercicio24 - Concatenar usando foldr
juntar :: [[a]] -> [a]
juntar [] = []
juntar l = foldr (++) [] l

-- Ejercicio25 - Función que obtiene el ultimo elemento de la lista.
init' :: [a] -> [a]
init' [] = error "Lista vacía"
init' [_] = []
init' (x:xs) = x : init' xs

-- Ejercicio26 - Definir la función nth tal que nth l n es elemento n–ésimo de l, empezando a numerar con el 0.
nth :: [a] -> Int -> a
nth [] _ = error "Lista vacía"
nth (x:_) 0 = x
nth (_:xs) n = nth xs (n-1)

-- Ejercicio27 - Definir la función lista_ordenada tal que lista_ordenada l se verifica si la lista l está ordenada de menor a mayor
listaOrdenada :: Ord a => [a] -> Bool
listaOrdenada [] = True
listaOrdenada [_] = True
listaOrdenada (x:y:xs) = (x <= y) && listaOrdenada (y:xs)

-- Ejercicio28 - Definir la función desde tal que desde n es la lista de los números enteros a partir de n.
desde :: Integer -> Integer -> [Integer]
desde n x
  | n > x = []
  | otherwise = n : desde (n+1) x

-- Ejercicio29 - Definir una función recursiva en Haskell que dada una lista de listas, devuelva una lista de enteros con las longitudes de cada lista.
longitudes :: [[a]] -> [Int]
longitudes [] = []
longitudes (x:xs) = length x : longitudes xs

-- Ejercicio30 - Definir una función que recibe tres calificaciones (0-100), calcula el promedio y finalmente indica cómo le fue en el curso (mal o bien):
-- mal: < 60
-- bien: >= 60 y <100
calificacion :: Int -> Int -> Int -> String
calificacion nota1 nota2 nota3
  | promedio < 60 = "mal"
  | promedio >= 60 && promedio < 100 = "bien"
  where promedio = (nota1 + nota2 + nota3) `div` 3

--Ejercicio31 - Definir una función en Haskell que dada una lista de enteros y un valor entero, indique si ese valor es menor que todos los de la lista.
menor :: [Int] -> Int -> Bool
menor [] valor = True
menor (x:xs) valor
    | valor >= x = False
    | otherwise = menor xs valor

--Ejercicio32 - Realizar una función en Haskell que dada una lista y un elemento, verifique si ese elemento pertenece a la lista.
pertenece :: Eq a => a -> [a] -> Bool
pertenece _ [] = False
pertenece x (y:ys)
  | x == y    = True
  | otherwise = pertenece x ys

--Ejercicio33 - Definir una función tal que dada una lista de enteros, genere la lista que represente su capicúa
invertirLista :: [a] -> [a]
invertirLista lista = reverse lista

--Ejercicio34 - Definir una función que dada una lista de enteros devuelva el mínimo de la lista.
minimoLista :: [Int] -> Int
minimoLista [] = error "La lista está vacía"
minimoLista [x] = x
minimoLista (x:xs) = min x (minimoLista xs)

--Ejercicio35 - Definir la función divisible tal que divisible x y se verifica si x es divisible por y, con x e y enteros.
divisible :: Integer -> Integer -> Bool
divisible x y = x `mod` y == 0

--Ejercicio36 - Definir la función multiplicar tal que multiplicar l es la multiplicación de los elementos de la lista l. Si la lista está vacía, debe devolver 1. La lista es de enteros.
multiplicarElementos :: [Integer] -> Integer
multiplicarElementos [] = 1
multiplicarElementos (x:xs) = x * multiplicarElementos xs

--Ejercicio37 - Definir la función "siguienteDoble" tal que siguienteDoble x retorna el doble del número que sigue a x, x es entero.
siguienteDoble :: Int -> Int
siguienteDoble x = (x + 1) * 2

--Ejercicio38 - Definir una función "iguales" tal que (iguales x y z) verifica si los elementos x, y y z son iguales, la función retorna una variable del tipo booleano.
iguales :: Eq a => a -> a -> a -> Bool
iguales x y z = x == y && y == z

--Ejercicio39 - Definir una función llamada tipoDeTriángulo, Dados tres números enteros que representan los lados de un triángulo, determinar si es equilátero, isósceles o escaleno utilizando guardas.
tipoDeTriangulo :: Int -> Int -> Int -> String
tipoDeTriangulo a b c
  | a == b && b == c = "Equilátero"
  | a == b || a == c || b == c = "Isósceles"
  | otherwise = "Escaleno"



main :: IO ()
main = do 
  -- Ejercicio: Mitad.
  let x = 8.5
  putStrLn "\n------------------------ Ejercicio 1 -------------------------"
  putStrLn ("La mitad de " ++ show x ++ " es: " ++ show (mitad x))

  -- Ejercicio "Suma de Nº en lista".
  putStrLn "\n------------------------ Ejercicio 2 -------------------------"
  let lista = [2,3,4]
  putStrLn ("La lista de números es: " ++ show lista)
  putStrLn ("La suma de los números de la lista es: " ++ show (sumar lista))

  -- Ejercicio Concatenar listas.
  putStrLn "\n------------------------ Ejercicio 3 -------------------------"
  let lista = [[1,2,3], [4,5], [], [1,2,8]]
  putStrLn $ "Lista original: " ++ show lista
  putStrLn $ "Lista concatenada: " ++ show (unir lista)

  -- Ejercicio EsPositivo.
  putStrLn "\n------------------------ Ejercicio 4 -------------------------"
  let num1 = 3
      num2 = -7
  putStrLn $ "¿Es " ++ show num1 ++ " positivo? " ++ show (esPositivo num1)
  putStrLn $ "¿Es " ++ show num2 ++ " positivo? " ++ show (esPositivo num2)

 -- Ejercicio Producto de los elementos de una lista.
  putStrLn "\n------------------------ Ejercicio 5 -------------------------"
  let lista = [1, 2, 3, 4, 5]
  putStrLn $ "Lista original: " ++ show lista
  putStrLn ("El producto de la lista es: " ++ show (producto lista))

-- Definir la función “or” tal que or l se verifica si algún elemento de l es verdadero.
  putStrLn "\n------------------------ Ejercicio 6 -------------------------"
  let lista1 = [1<2, 2<3, 1 /= 0]
  putStrLn ("¿Alguno de los elementos es verdadero? " ++ show (or' lista1))
  let lista2 = [3<2, 4<3, 1 == 0]
  putStrLn ("¿Alguno de los elementos es verdadero? " ++ show (or' lista2))

-- Invertir una lista: El operador Ord me sirve para indicar que representa a cualquier tipo de dato.
  putStrLn "\n------------------------ Ejercicio 7 -------------------------"
  let lista = [5,4,7,8]
  putStrLn ("La lista original: " ++ show lista)
  putStrLn ("La lista invertida es: " ++ show (invertir lista))

-- Función comparar si 2 listas son iguales:
  putStrLn "\n------------------------ Ejercicio 8 -------------------------"
  let lista1 = [1,2,3,4,5]
  let lista2 = [1,2,3,4,5]
  let lista3 = [1,2,3]
  putStrLn ("¿La lista 1 es igual a la lista 2? " ++ show (sonIguales lista1 lista2))
  putStrLn ("¿La lista 1 es igual a la lista 3? " ++ show (sonIguales lista1 lista3))

-- Solicita al usuario ingresar un número y luego muestra en pantalla el número siguiente:
  putStrLn "\n------------------------ Ejercicio 9 -------------------------"
  --putStr "Ingresa un numero: "
  --entrada <- getLine
  --let numero = read entrada :: Int
  let numero = 5
  putStrLn ("El siguiente número es: " ++ show (siguiente numero)) 

-- Busca posición de letra s en un string de caracteres.
  putStrLn "\n----------------------- Ejercicio 10 -------------------------"
  putStrLn ("La letra s está en la posición: " ++ show (buscarLetra listaDeLetras "s"))

-- Eleva al cuadrado cada uno de los elementos que contiene la lista.
  putStrLn "\n----------------------- Ejercicio 11 -------------------------"
  let listaOriginal = [6,7,8,9,10,12] --Lista original de números enteros.
  let listaCuadrada = numCuadrado listaOriginal --Aplica función al cuadrado de la lista original.
  putStrLn ("Lista original: " ++ show listaOriginal) --Imprime lista original.
  putStrLn ("Lista elevada al cuadrado: " ++ show listaCuadrada) --Imprime lista al cuadrado.

-- Filtra valores pares de una lista mayores a 10.
  putStrLn "\n----------------------- Ejercicio 12 -------------------------"
  let lista = [1..100] --Lista con valores del 1 al 100.
  let sublista = filtrarPar lista --Lista filtrada. 
  putStrLn ("Los números pares mayores a 10 son: " ++ show sublista ++ ".")

-- Suma de cuadrados de una lista.
  putStrLn "\n----------------------- Ejercicio 13 -------------------------"
  let lista = [1,2,3,4,5]
  let resultado = sumaCuadrados lista
  putStrLn ("Lista original: " ++ show lista) -- Lista original.
  putStrLn ("Suma de los cuadrados: " ++ show resultado ++ ".") -- Lista el resultado de la suma.

-- Rango de edad entre 71 y 79 años.  
  putStrLn "\n----------------------- Ejercicio 14 -------------------------"
  let edad1 = 70
      edad2 = 78
  putStrLn "Ejecución de los rangos de edad de 71 y 79:"
  putStrLn $ show edad1 ++ " - " ++ rangoEdad edad1
  putStrLn $ show edad2 ++ " - " ++ rangoEdad edad2

-- VERIFICAR VERIFICAR VERIFICAR VERIFICAR VERIFICAR VERIFICAR VERIFICAR VERIFICAR VERIFICAR VERIFICAR VERIFICAR
  putStrLn "\n----------------------- Ejercicio 15 -------------------------"
  let edades = [70..85]
  let n = 6
  putStrLn $ "Lista de edades: " ++ show edades
  print (filtrarRangoEtario edades n)

-- Dada una lista de enteros y un valor, devuelva los elementos que multiplicados por 3 son mayores a 5
  putStrLn "\n----------------------- Ejercicio 16 -------------------------"
  let lista = [1..10]
  let valor = 5
  putStrLn ("Elementos que multiplicados por 3 son mayores a 5: " ++ show (filtrarMultiplicadosPor3MayoresA5 lista))

-- Hacer una función que me permita generar una lista (a partir de la proporcionada) donde cada elemento en la salida sea de tipo tupla de dos componentes (según las siguientes características): [( x , 'X' ), …] donde 'X' es el carácter que le corresponde al ascii=x
  putStrLn "\n----------------------- Ejercicio 17 -------------------------"
  let asciiTuples = [(n, chr n) | n <- [1..100]]
  mapM_ print asciiTuples

-- Definir una funcion en haskell que permita ingresar un numero cualquiera y muestre en consola si es primo o no.
  putStrLn "\n----------------------- Ejercicio 18 -------------------------"
  let num = 4
  if esPrimo num
    then putStrLn ("El número: " ++ show num ++ " es primo.")
    else putStrLn ("El número: " ++ show num ++ " no es primo.")

-- Definir la función ultimaCifra tal que (ultimaCifra x) es la última cifra del número.
  putStrLn "\n----------------------- Ejercicio 19 -------------------------"
  let numero = 156224
  putStrLn ("El número: " ++ show numero ++ " tiene como última cifra el número: " ++ show (ultimaCifra numero))

-- Definir en Haskell la función rotar tal que (rotar xs) es la lista obtenida poniendo el primer elemento de xs al final de la lista. Cambia el primer elemento para el final de la lista original.
  putStrLn "\n----------------------- Ejercicio 20 -------------------------"
  let lista = [3, 2, 5, 7]
  putStrLn ("Lista original: " ++ show lista)
  putStrLn ("Lista rotada: " ++ show (rotar lista))

-- Definir la función numeroDeDigitos tal que (numeroDeDigitos x) es el número de dígitos de x.
  putStrLn "\n----------------------- Ejercicio 21 -------------------------"
  let numero = 12345678
  putStrLn ("Número ingresado: " ++ show numero)
  putStrLn ("Cantidad de dígitos: " ++ show (numeroDeDigitos numero))

-- Definir la función capicua tal que (capicua n) se verifica si los dígitos de n son los mismos de izquierda a derecha que de derecha a izquierda.
  putStrLn "\n----------------------- Ejercicio 22 -------------------------"
  let numero = 121
  if capicua numero
    then putStrLn (show numero ++ " es capicúa.")
    else putStrLn (show numero ++ " no es capicúa.")

-- Definir la función head tal que head l es la cabeza de la lista l.
  putStrLn "\n----------------------- Ejercicio 23 -------------------------"
  let lista1 = [8,1,5]  :: [Int]
  putStrLn ("La cabeza de la lista: " ++ show lista1 ++ " es: " ++ show (headLista lista1) ++ ".")

-- Definir la función juntar tal que foldr l es la concatenación de las listas de l.
  putStrLn "\n------------------------ Ejercicio 24 -------------------------"
  let lista = [[1,2,3], [4,5], [], [1,2,8]]
  putStrLn $ "Lista original: " ++ show lista
  putStrLn $ "Lista concatenada: " ++ show (juntar lista)

-- Definir la función init tal que init l es la lista l sin el último elemento.
  putStrLn "\n------------------------ Ejercicio 25 -------------------------"
  let lista = [1,2,3,4,5,6]
  putStrLn $ "Lista original: " ++ show lista
  putStrLn $ "Lista sin último elemento: " ++ show (init' lista)

-- Definir la función nth tal que nth l n es elemento n–ésimo de l, empezando a numerar con el 0.
  putStrLn "\n------------------------ Ejercicio 26 -------------------------"
  let lista = [1,3,2,4,9,7]
  let numero = 4
  putStrLn $ "Lista original: " ++ show lista
  putStrLn $ "El elemento n-ésimo es: " ++ show numero ++ " y el valor en la lista es: " ++ show (nth lista numero) ++ "."

-- Definir la función lista_ordenada tal que lista_ordenada l se verifica si la lista l está ordenada de menor a mayor
  putStrLn "\n------------------------ Ejercicio 27 -------------------------"
  let lista = [1,2,3,4,5,6,7,8,9]
  putStrLn $ "Lista original: " ++ show lista
  putStrLn $ "Lista ordenada: " ++ show (listaOrdenada lista)

-- Definir la función desde tal que desde n es la lista de los números enteros a partir de n.
  putStrLn "\n------------------------ Ejercicio 28 -------------------------"
  let numeroDesde = 8
  let numeroHasta = 15
  putStrLn $ "Lista de número desde: " ++ show numeroDesde ++ " hasta: " ++ show numeroHasta ++ ".\n" ++ show (desde numeroDesde numeroHasta)

-- Definir una función recursiva en Haskell que dada una lista de listas, devuelva una lista de enteros con las longitudes de cada lista.
  putStrLn "\n------------------------ Ejercicio 29 -------------------------"
  print $ longitudes ["hola"]
  print $ longitudes [[1, 2], [3]]
  print $ longitudes [[1, 2, 3], [4, 5], [6, 7, 8, 9]]

-- Definir una función que recibe tres calificaciones (0-100), calcula el promedio y finalmente indica cómo le fue en el curso (mal o bien):
-- mal: < 60
-- bien: >= 60 y <100
  putStrLn "\n------------------------ Ejercicio 30 -------------------------"
  putStrLn (calificacion 50 70 60) -- "bien"
  putStrLn (calificacion 50 70 50) -- "mal"

--Definir una función en Haskell que dada una lista de enteros y un valor entero, indique si ese valor es menor que todos los de la lista.
  putStrLn "\n------------------------ Ejercicio 31 -------------------------"
  print (menor [5,8,9] 1)   -- True
  print (menor [5,8,9] 15)  -- False
  print (menor [3,6,2] 2)   -- False
  print (menor [7,10,14] 6) -- True

--Realizar una función que dada una lista y un elemento, verifique si ese elemento pertenece a la lista.
  putStrLn "\n------------------------ Ejercicio 32 -------------------------"
  let lista = [1, 2, 3, 4, 5]
  let elemento1 = 3
  let elemento2 = 6
  putStrLn $ "Lista: " ++ show lista
  if pertenece elemento1 lista
    then putStrLn $ show elemento1 ++ " pertenece a la lista."
    else putStrLn $ show elemento1 ++ " no pertenece a la lista."

  if pertenece elemento2 lista
    then putStrLn $ show elemento2 ++ " pertenece a la lista."
    else putStrLn $ show elemento2 ++ " no pertenece a la lista."

-- Definir una función tal que dada una lista de enteros, genere la lista que represente su capicúa.
  putStrLn "\n------------------------ Ejercicio 33 -------------------------"
  let lista = ['a', 'b', 'c', 'd', 'g']
  putStrLn $ "Lista original:" ++ show lista
  putStrLn $ "Lista capicúa:" ++ show (invertirLista lista)

--Definir una función que dada una lista de enteros devuelva el mínimo de la lista.
  putStrLn "\n------------------------ Ejercicio 34 -------------------------"
  let lista = [5, 3, 8, 1, 6]
  putStrLn $ "Lista original:" ++ show lista
  putStrLn $ "Elemento con valor mínimo: " ++ show (minimoLista lista)

--Definir la función divisible tal que divisible x y se verifica si x es divisible por y, con x e y enteros.
  putStrLn "\n------------------------ Ejercicio 35 -------------------------"
  putStrLn "¿Es 3 divisible por 5?"
  putStrLn (if divisible 3 5 then "Verdadero" else "Falso")
  putStrLn "¿Es 4 divisible por 2?"
  putStrLn (if divisible 4 2 then "Verdadero" else "Falso")

--Definir la función multiplicar tal que multiplicar l es la multiplicación de los elementos de la lista l. Si la lista está vacía, debe devolver 1. La lista es de enteros.
  putStrLn "\n------------------------ Ejercicio 36 -------------------------"
  putStrLn "Multiplicar elementos de la lista [1, 3, 6]:"
  print (multiplicarElementos [1, 3, 6])
  putStrLn "Multiplicar elementos de la lista vacía:"
  print (multiplicarElementos [])

--Definir la función "siguienteDoble" tal que siguienteDoble x retorna el doble del número que sigue a x, x es entero.
  putStrLn "\n------------------------ Ejercicio 37 -------------------------"
  putStrLn "Ingrese un número y mostrará el doble del siguiente:" 
  input <- getLine
  let number = read input :: Int
  let result = siguienteDoble number
  putStrLn ("El doble del número siguiente es: " ++ show result)

--Definir en Haskell una función "iguales" tal que (iguales x y z) verifica si los elementos x, y y z son iguales, la función retorna una variable del tipo booleano.
  putStrLn "\n------------------------ Ejercicio 38 -------------------------"
  putStrLn "Ingrese tres elementos:"
  x <- getLine
  y <- getLine
  z <- getLine
  let result = iguales x y z
  putStrLn ("Los elementos son iguales: " ++ show result)

--Definir una función llamada tipoDeTriángulo, Dados tres números enteros que representan los lados de un triángulo, determinar si es equilátero, isósceles o escaleno utilizando guardas.
  putStrLn "\n------------------------ Ejercicio 39 -------------------------"
  putStrLn "Ingrese los tres lados del triángulo:"
  a <- readLn
  b <- readLn
  c <- readLn
  let result = tipoDeTriangulo a b c
  putStrLn ("El triángulo es de tipo: " ++ result)
