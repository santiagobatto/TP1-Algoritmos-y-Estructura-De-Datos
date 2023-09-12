--Batto Justo Santiago, Algoritmos y estructura de datos, DNI: 44812542

-- 1. Programa las siguientes funciones:
-- a) esCero :: Int -> Bool, que verifica si un entero es igual a 0.
esCero :: Int -> Bool
esCero x = x == 0
-- ghci> esCero 3
-- False
-- ghci> esCero 0
-- True

-- b) esPositivo :: Int -> Bool, que verifica si un entero es estrictamente mayor a 0.
esPositivo :: Int -> Bool
esPositivo x = x > 0
-- ghci> esPositivo 4
-- True
-- ghci> esPositivo 0
-- False
-- ghci> esPositivo (-2)
-- False

-- c) esVocal :: Char -> Bool, que verifica si un caracter es una vocal en minuscula.
esVocal :: Char -> Bool
esVocal x | x == 'a' = True | x == 'e' = True | x == 'i' = True| x == 'o' = True| x == 'u' = True | otherwise = False
-- ghci> esVocal 'p'
-- False
-- ghci> esVocal 'A'
-- False
-- ghci> esVocal 'i'
-- True
-- ghci> esVocal 'W'
-- False

-- d) valorAbsoluto :: Int -> Int, que devuelve el valor absoluto de un entero ingresado.
valorAbsoluto :: Int -> Int
valorAbsoluto x | x >= 0 = x | x < 0 = -x
-- ghci> valorAbsoluto 8
-- 8
-- ghci> valorAbsoluto 0
-- 0
-- ghci> valorAbsoluto (-3)
-- 3

-- 2. Programa las siguientes funciones usando recursion o composicion:

-- a) paratodo :: [Bool] -> Bool, que verifica que todos los elementos de una lista sean True.
paraTodo :: [Bool] -> Bool
paraTodo [] = True
paraTodo (x:xs) = x && paraTodo xs
-- ghci> paraTodo [False, True]
-- False
-- ghci> paraTodo [True, False, True]
-- False
-- ghci> paraTodo [True, True]
-- True

-- b) sumatoria :: [Int] -> Int, que calcula la suma de todos los elementos de una lista de enteros.
sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs
-- ghci> sumatoria [2,4,7,8] 
-- 21
-- ghci> sumatoria [(-3),4,0]
-- 1

-- c) productoria :: [Int] -> Int, que calcula el producto de todos los elementos de la lista de enteros.
productoria :: [Int] -> Int
productoria [] = 1
productoria (x:xs) = x * productoria xs
-- ghci> productoria [2,3,7]
-- 42
-- ghci> productoria [3,5,(-1)]
-- -15

-- d) factorial :: Int -> Int, que toma un numero n y calcula n!.
factorial :: Int -> Int
factorial 0 = 1
factorial 1 = 1
factorial x = x * factorial (x-1)
-- ghci> factorial 4
-- 24
-- ghci> factorial 0
-- 1
-- ghci> factorial 1 
-- 1

-- e) Utiliza la funcion sumatoria para definir, promedio :: [Int] -> Int, que toma una lista de numeros no vacia y calcula el valor promedio (truncado, usando division entera).
promedio :: [Int] -> Int
promedio [] = 1
promedio xs = div (sumatoria xs) (length xs)
-- ghci> promedio [7,8,2]
-- 5
-- ghci> promedio [3,6,9]
-- 6

-- 3. Programa la funcion pertenece :: Int -> [Int] -> Bool, que verifica si un numero se encuentra en una lista.

pertenece :: Int -> [Int] -> Bool
pertenece a [] = False
pertenece a (x:xs) = (a == x) && pertenece a xs
-- ghci> pertenece 2 []
-- False
-- ghci> pertenece 2 [3,6,8]
-- False
-- ghci> pertenece 2 [2,6,8]
-- True
-- ghci> pertenece 2 [4,6,2]
-- True

-- 4 Programá las siguientes funciones que implementan los cuantificadores generales. Nota que el segundo parametro de cada funcion, es otra funcion!

-- a) paratodo’ :: [a] -> (a -> Bool) -> Bool, dada una lista xs de tipo [a] y un predicado t :: a -> Bool, determina si todos los elementos de xs satisfacen el predicado t.
paratodo' :: [a] -> (a -> Bool) -> Bool
paratodo' [] t = True
paratodo' (x:xs) t = t x && paratodo' xs t
-- ghci> paratodo' ['a','z','x','u'] esVocal
-- False
-- ghci> paratodo' ['a','u'] esVocal        
-- True

-- b) existe’ :: [a] -> (a -> Bool) -> Bool, dada una lista xs de tipo [a] y un predicado t :: a -> Bool, determina si algun elemento de xs satisface el predicado t.
existe' :: [a] -> (a -> Bool) -> Bool
existe' [] t = False
existe' (x:xs) t = t x || existe' xs t
-- ghci> existe' [3,4,0,5] esCero
-- True
-- ghci> existe' [3,4,5] esCero  
-- False

-- c) sumatoria’ :: [a] -> (a -> Int) -> Int, dada una lista xs de tipo [a] y una funcion t :: a -> Int (toma elementos de tipo a y devuelve enteros), calcula la suma de los valores que resultan de la aplicacion de t a los elementos de xs.
sumatoria' :: [a] -> (a -> Int) -> Int
sumatoria' [] t = 0
sumatoria' (x:xs) t = t x + sumatoria' xs t
-- ghci> sumatoria' [3,5,(-6),0] valorAbsoluto 
-- 14
-- ghci> sumatoria' [(-21),(-4), 2] valorAbsoluto 
-- 27

-- d) productoria’ :: [a] -> (a -> Int) -> Int, dada una lista de xs de tipo [a] y una funcion t :: a -> Int, calcula el producto de los valores que resultan de la aplicacion de t a los elementos de xs.
productoria' :: [a] -> (a -> Int) -> Int
productoria' [] t = 1
productoria' (x:xs) t = t x * productoria' xs t

-- Usare la funcion succ, predefinida por el Prelude. Esta funcion suma 1 al numero que le pase como parametro.

-- ghci> productoria' [2,5,(-4)] succ
-- -54
-- ghci> productoria' [2,5,0] succ   
-- 18

-- 5 Defini nuevamente la funcion paratodo, pero esta vez usando la funcion paratodo’ (sin recursíon ni analisis por casos!).
 
paratodo :: [a] -> (a -> Bool) -> Bool
paratodo xs t = paratodo' xs t
-- ghci> paratodo ['a','e','j'] esVocal 
-- False
-- ghci> paratodo ['a','e'] esVocal     
-- True

-- 6 Utilizando las funciones del ejercicio 4, programa las siguientes funciones por composicion, sin usar recursion ni analisis por casos.

-- a) todosPares :: [Int] -> Bool verifica que todos los numeros de una lista sean pares.

esPar :: Int -> Bool        -- ghci> esPar 0 
esPar x = even x            -- True
                            -- ghci> esPar 3
                            -- False

todosPares :: [Int] -> Bool
todosPares xs = paratodo' xs esPar
-- ghci> todosPares [-2,18,36]
-- True
-- ghci> todosPares [24,-7,26]
-- False

-- b) hayMultiplo :: Int -> [Int] -> Bool verifica si existe algun numero dentro del segundo parametro que sea multiplo del primer parametro.

multiplo :: Int -> Int -> Bool          -- ghci> multiplo 2 4
multiplo x y = mod y x == 0             -- False
                                        -- ghci> multiplo 12 6
                                        -- True

hayMultiplo :: Int -> [Int] -> Bool
hayMultiplo x xs = existe' xs (multiplo x) 
-- ghci> hayMultiplo 7 [5, 22, 49, 13]
-- True
-- ghci> hayMultiplo 7 [3, 48, 15]
-- False

--  sumaCuadrados :: Int -> Int, dado un numero no negativo n, calcula la suma de los primeros n cuadrados.

cuadrado :: Int -> Int          -- ghci> cuadrado 3     
cuadrado x = x*x                -- 9
                                -- ghci> cuadrado 7
                                -- 49

sumaCuadrados :: Int -> Int
sumaCuadrados n =  sumatoria' [0..(n-1)] cuadrado
-- ghci> sumaCuadrados 21
-- 2870
-- ghci> sumaCuadrados 3
-- 5

-- d) Programar la fucion existeDivisor :: Int -> [Int] -> Bool, que dado en entero n y una lista ls , devuelve True si y solo si, existe algun elemento en ls que divida a na.

divide :: Int -> Int -> Bool        -- ghci> divide 4 12
divide x y = mod x y == 0           -- False
                                    -- ghci> divide 21 4
                                    -- False

existeDivisor :: Int -> [Int] -> Bool
existeDivisor n xs = existe' xs (divide n)
-- ghci> existeDivisor 3 [5,17,8]   
-- False
-- ghci> existeDivisor 4 [31,13,2,5]
-- True

-- e) Utilizando la funcion del apartado anterior, definı la funcion esPrimo:: Int -> Bool, que dado un entero n, devuelve True si y solo si n es primo.

esPrimo:: Int -> Bool
esPrimo n = n > 1 && not (existeDivisor n [2..(n-1)])
-- ghci> esPrimo 4 
-- False
-- ghci> esPrimo 5
-- True

-- f ) ¿Se te ocurre como redefinir factorial (ej. 2d) para evitar usar recursion?

factorialSinRecursion :: Int -> Int
factorialSinRecursion 0 = 1
factorialSinRecursion n = productoria' [1..n] id

-- g) Programar la funcion multiplicaPrimos :: [Int] -> Int que calcula el producto de todos los numeros primos de una lista.

multiplicaPrimos :: [Int] -> Int
multiplicaPrimos xs = productoria' (filter esPrimo xs) id
-- ghci> multiplicaPrimos [3,6,8,2,11]
-- 66
-- ghci> multiplicaPrimos [6,8,12]    
-- 1

-- h) Programar la funcion esFib :: Int -> Bool, que dado un entero n, devuelve True si y solo si n esta en la sucesion de Fibonacci.

fibonacci  :: Int -> Int    -- Devuelve el resultado de la serie Fibonacci en funcion de n
fibonacci n | n == 0 = 0
            | n == 1 = 1
            | otherwise = fibonacci (n-1) + fibonacci (n-2)
-- ghci> fibonacci 4
-- 3
-- ghci> fibonacci 7
-- 13

listaFibonacci :: Int -> [Int]    -- Devuelve la lista de la sucesion Fibonacci hasta la posicion n
listaFibonacci n | n < 5 = map fibonacci [0..(n+2)]
                 | otherwise = map fibonacci [0..n]
-- ghci> listaFib 6
-- [0,1,1,2,3,5,8]
-- ghci> listaFib 4
-- [0,1,1,2,3,5,8]

perteneceA :: [Int] -> Int -> Bool  -- Comprueba si un elemento pertenece a la lista
perteneceA [] n = False
perteneceA (x:xs) n | x == n = True
                    | otherwise = perteneceA xs n
-- ghci> perteneceA [1,3,5,8,14] 7
-- False
-- ghci> perteneceA [1,3,5,8,14] 3
-- True

esFib :: Int -> Bool
esFib n = perteneceA (listaFibonacci n) n   -- LLama a la funcion perteneceA y le pasa por parametro la lista que devuelve listaFibonacci, y verifica si n esta dentro de la lista
-- ghci> esFib 4 
-- False
-- ghci> esFib 5
-- True
-- ghci> esFib 21
-- True

-- i) Utilizando la funcion del apartado anterior, definı la funcion todosFib :: [Int] -> Bool que dada una lista xs de enteros, devuelva si todos los elementos de la lista pertenecen (o no) a la sucesion de Fibonacci.

todosFib :: [Int] -> Bool
todosFib [] = True
todosFib (x:xs) = esFib x && todosFib xs
-- ghci> todosFib [1,2,3,5,8,13,21]
-- True
-- ghci> todosFib [1,2,3,5,7,13,34]
-- False

{- 7. a) Que hacen las funciones map y filter?

    Map: Su sintaxis:    map :: (a -> b) -> [a] -> [b]      es la lista obtenida al aplicar una funcion f a cada uno de los elementos de una lista xs.
    Filter: Su sintaxis:    filter :: (a -> Bool) -> [a] -> [a]     es una funcion que aplicada a un predicado y una lista, devuelve la lista de los elementos que satisfagan con el predicado.

    La expresion map succ [1, -4, 6, 2, -8], donde succ n = n+1 equivale a la lista [2,-3,7,3,-7]

    Y filter esPositivo [1, -4, 6, 2, -8] equivale a [1,6,2]

-}

-- 8. Programa una funcion que dada una lista de numeros xs, devuelve la lista que resulta de duplicar cada valor de xs.

duplicar :: [Int] -> [Int]          -- a) Usando recursion
duplicar [] = []
duplicar (x:xs) = x * 2 : duplicar xs
-- ghci> duplicar [7,21,10] 
-- [14,42,20]
-- ghci> duplicar [3,5,12]
-- [6,10,24]

duplica :: Int -> Int
duplica n = n*2
-- ghci> duplica 3
-- 6
-- ghci> duplica 9
-- 18

duplicarMap :: [Int] -> [Int]       -- b) Usando map
duplicarMap xs = map duplica xs
-- ghci> duplicarMap [7,23,11]
-- [14,46,22]
-- ghci> duplicarMap [21,77,5]
-- [42,154,10]
 
-- 9. Programa una funcion que dada una lista de numeros xs, calcula una lista que tiene como elementos aquellos numeros de xs que son primos.

listaPrimos :: [Int] -> [Int]        -- a) Usando recursion
listaPrimos [] = []
listaPrimos (x:xs) | esPrimo x == True = x : listaPrimos xs
                   | not (esPrimo x) = listaPrimos xs
-- ghci> soloPrimos [4,6,2,7,11,24]
-- [2,7,11]
-- ghci> soloPrimos [8,24,3,10]    
-- [3]

listaPrimosFilter :: [Int] -> [Int]     -- b) Usando filter
listaPrimosFilter xs = filter esPrimo xs
-- ghci> listaPrimosFilter [6,8,12,9]
-- []
-- ghci> listaPrimosFilter [3,24,17,9,5]
-- [3,17,5]

multiplicaPrimosOptima :: [Int] -> Int      -- c) Mejore multiplica primos usando product (funcion definida en el prelude) 
multiplicaPrimosOptima xs = product (filter esPrimo xs)
-- ghci> multiplicaPrimosOptima [2,7,28,3,10]
-- 42
-- ghci> multiplicaPrimosOptima [18,9,7,41]  
-- 287

-- 10. La funcion primIgualesA toma un valor y una lista, y calcula el tramo inicial mas largo de la lista cuyos elementos son iguales a ese valor.

primIgualesA :: forall a. Eq a => a -> [a] -> [a]   -- a) Usando recursion
primIgualesA a [] = []
primIgualesA a (x:xs) | a == x = x : primIgualesA a xs
                      | otherwise = primIgualesA a []
-- ghci> primIgualesA 5 [5,5,5,4,5,4,8] 
-- [5,5,5]
-- ghci> primIgualesA 's' ['s','s','a','n']      
-- "ss"
-- ghci> primIgualesA 'a' "aaaalgoritmos"
-- "aaaa"

takeWhilePrimIgualesA :: forall a. Eq a => a -> [a] -> [a]   -- b) Usando takeWhile
takeWhilePrimIgualesA a xs = takeWhile (== a) xs
-- ghci> takeWhilePrimIgualesA 8 [8,8,7,13]
-- [8,8]
-- ghci> takeWhilePrimIgualesA 'y' "yessssssss"
-- "y"

-- 11. La funcion primIguales toma una lista y devuelve el mayor tramo inicial de la lista cuyos elementos son todos iguales entre si. 

primIguales :: forall a. Eq a => [a] -> [a]         -- a) Usando recursion
primIguales [] = []
primIguales (x:y:xs) | x == y = x : primIguales (y:xs)
                     | otherwise = primIguales []
-- ghci> primIguales [2,6,84]
-- []
-- ghci> primIguales [9,9,9,21,5,7]
-- [9,9]

primIguales' :: forall a. Eq a => [a] -> [a]       -- b) Usando primIgualesA
primIguales' (x:xs) = takeWhilePrimIgualesA x (x:xs)
-- ghci> primIguales' [4,6,2,87]
-- [4]
-- ghci> primIguales' [5,5,5,5,23,7,98]
-- [5,5,5,5]