sumatoria :: Int -> Int
sumatoria 0 = 0
sumatoria n = n + sumatoria (n-1)

sumatoria' :: Int -> Int
sumatoria' n = div (n*(n+1)) 2      -- Por Gauss, sin recursión

f1 :: Int -> Int
f1 0 = 1
f1 n = 2^n + f1 (n-1)

f2 :: Int -> Float -> Float
f2 0 q = 0
f2 n q = q^n + f2 (n-1) q

f3 :: Int -> Float -> Float
f3 0 q = 0
f3 n q = q^(2*n) + q^(2*n-1) + f3 (n-1) q

f3' :: Int -> Float -> Float
f3' n q = f2 (2*n) q

f4 :: Int -> Float -> Float
f4 0 q = 0
f4 n q = q^(2*n) + q^(2*n-1) - q^(n-1) + f4 (n-1) q     -- Resto el q^(n-1) que tengo que sumar para encontrar f4 (n-1) q

f4' :: Int -> Float -> Float
f4' n q = f3 n q - f2 (n-1) q

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

eAprox :: Int -> Float
eAprox 0 = 1
eAprox n = (1 / (fromIntegral (factorial n))) + eAprox (n-1)    -- fromIntegral hace el casteo a Float

e :: Float
e = eAprox 10

f :: Int -> Int -> Int
f 0 m = 0
f n m = round (f2 m (fromIntegral n)) + (f (n-1) m)

sumaPotencias :: Float -> Int -> Int -> Float
sumaPotencias q n 0 = 0
sumaPotencias q n m = (sumaPotencias q n (m-1)) + q^m * (f2 n q)

sumaRacionales :: Int -> Int -> Float
sumaRacionales n 0 = 0
sumaRacionales n m = (sumaRacionales n (m-1)) + fromIntegral((sumatoria n)) / (fromIntegral m)

g1 :: Int -> Int -> Int
g1 i n
    | n < i     = undefined
    | n == i    = i^i
    | otherwise = i^(n) + g1 i (n-1)

g2 :: Int -> Int
g2 n
    | n == 1    = 1
    | otherwise = (sumaPotenciasN n n) + g2 (n-1)

sumaPotenciasN :: Int -> Int -> Int
sumaPotenciasN i n
    | i == 0    = 0
    | otherwise = i^n + (sumaPotenciasN (i-1) n)

g3 :: Int -> Int
g3 n
    | n == 0       = 0
    | mod n 2 == 0 = 2^n + g3 (n-2)
    | otherwise    = g3 (n-1)

sumaTodosIguales :: Int -> Int
sumaTodosIguales n
    | n == 0         = 0
    | todosIguales n = n + sumaTodosIguales (n-1)
    | otherwise      = sumaTodosIguales (n-1)

todosIguales :: Int -> Bool
todosIguales n
    | n < 0     = undefined
    | n < 10    = True
    | otherwise = ultimoDigito n == anteultimoDigito n && todosIguales (sacarUltimoDigito n)

ultimoDigito :: Int -> Int
ultimoDigito n = mod n 10

sacarUltimoDigito :: Int -> Int
sacarUltimoDigito n = div n 10

anteultimoDigito :: Int -> Int
anteultimoDigito n = ultimoDigito (sacarUltimoDigito n)
