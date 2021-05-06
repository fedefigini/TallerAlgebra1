factorial :: Integer -> Integer             -- Enteros infinitamente largos simulados por soft => Más costoso de operar
factorial n
    | n == 0    = 1
    | n >  0    = n * factorial (n-1)
    | n <  0    = undefined
    | otherwise = n * factorial (n-1)       -- Fallaría para negativos por stack overflow

{-
factorial 0 = 1
factorial n = n * factorial (n-1)           -- Con pattern matching, same del overflow
-}

esPar :: Int -> Bool
esPar n
    | n == 0    = True
    | n == 1    = False                     -- Por restar de a 2 necesito un 2do caso base
    | otherwise = esPar (n-2)

{-
esPar :: Int -> Bool
esPar n | n == 0    = True                  -- Un solo caso base por restar de a 1
        | otherwise = not (esPar (n-1))
-}

fibonacci :: Int -> Int
fibonacci n
    | n == 0    = 0
    | n == 1    = 1
    | otherwise = fibonacci (n-1) + fibonacci (n-2)

parteEntera :: Float -> Int
parteEntera n
    | n < 0     = undefined
    | n < 1     = 0
    | otherwise = 1 + parteEntera (n-1)

esMultiploDe3 :: Int -> Bool
esMultiploDe3 n
    | n == 0    = True
    | n == 1    = False
    | n == 2    = False
    | otherwise = esMultiploDe3 (n-3)

sumaImpares :: Int -> Int
sumaImpares n
    | n == 1    = 1
    | otherwise = 2*n - 1  + sumaImpares(n - 1)

medioFact :: Int -> Int
medioFact n
    | n < 0     = undefined
    | n == 0    = 1
    | n == 1    = 1
    | otherwise = n * medioFact(n-2)

sumaDigitos :: Int -> Int
sumaDigitos n
    | n < 0     = undefined
    | n < 10    = n
    | otherwise = ultimoDigito n + sumaDigitos (sacarUltimoDigito n)

ultimoDigito :: Int -> Int
ultimoDigito n = mod n 10

sacarUltimoDigito :: Int -> Int
sacarUltimoDigito n = div n 10

todosIguales :: Int -> Bool
todosIguales n
    | n < 0     = undefined
    | n < 10    = True
    | otherwise = ultimoDigito n == anteultimoDigito n && todosIguales (sacarUltimoDigito n)

anteultimoDigito :: Int -> Int
anteultimoDigito n = ultimoDigito (sacarUltimoDigito n)
