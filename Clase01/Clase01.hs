-- f x y = x*x + y*y

g x y z = x + y + z*z

doble x = 2*x

suma x y = x + y

normaVectorial x1 x2 = sqrt(x1^2 + x2^2)

funcionConstante8 x = 8

{-
-- reescritas por pattern matching
f n | n == 0 = 1
    | otherwise = 0     -- n /= 0 = 0

signo n | n > 0 = 1
        | n == 0 = 0
        | n < 0 = -1
-}

maximo :: Int -> Int -> Int
maximo x y
     | x >= y    = x
     | otherwise = y

maximoRac :: Float -> Float -> Float
maximoRac x y
     | x >= y = x
     | otherwise = y

f1 n | n >= 3 = 5

f2 n | n >= 3 = 5
     | n <= 1 = 8

f3 n | n >= 3    = 5
     | n == 2    = undefined
     | otherwise = 8

f4 n | n >= 3 = 5
     | n <= 9 = 7

f5 n | n <= 9 = 7
     | n >= 3 = 5


-- pattern matching

f 0 = 1
f n = 0

signo 0 = 0
signo n
     | n > 0     = 1
     | otherwise = -1

cantidadDeSoluciones b c
     | d > 0     = 2
     | d == 0    = 1
     | otherwise = 0
     where d = b^2 - 4*c

esMayorA9 :: Int -> Bool
esMayorA9 n
     | n > 9     = True
     | otherwise = False 

esPar :: Int -> Bool
esPar n
     | mod n 2 == 0 = True
     | otherwise    = False

esPar2 :: Int -> Bool
esPar2 n = mod n 2 == 0

esImpar :: Int -> Bool
esImpar n = not ( esPar n )

{-
funcionRara :: Float -> Float -> Bool -> Bool
funcionRara x y z = (x >= y) || z
-}

{-
funcionRara :: Float -> Float -> Bool -> Bool
funcionRara x y True = True
funcionRara x y False = x >= y
-}

funcionRara :: Float -> Float -> Bool -> Bool
funcionRara _ _ True = True
funcionRara x y False = x >= y


-- tarea

absoluto :: Int -> Int
absoluto x
     | x >= 0    = x
     | otherwise = (-x)

maximoAbsoluto :: Int -> Int -> Int
maximoAbsoluto x y
     | absX > absY = absX
     | otherwise   = absY
     where absX = absoluto x
           absY = absoluto y

maximo3 :: Int -> Int -> Int -> Int
maximo3 x y z
     | x > m     = x
     | otherwise = m
     where m = maximo y z

algunoEs0 :: Float -> Float -> Bool     -- Lógica
algunoEs0 x y = (x == 0) || (y == 0)

algunoEs0M :: Float -> Float -> Bool    -- M: Matemática
algunoEs0M x y = x * y == 0

algunoEs0PM :: Float -> Float -> Bool   -- PM: Pattern Matching
algunoEs0PM 0 _ = True
algunoEs0PM _ 0 = True
algunoEs0PM _ _ = False

ambosSon0 :: Float -> Float -> Bool     -- Lógica
ambosSon0 x y = (x == 0) && (y == 0)

ambosSon0M :: Float -> Float -> Bool    -- M: Matemática
ambosSon0M x y = x^2 + y^2 == 0

ambosSon0PM :: Float -> Float -> Bool   -- PM: Pattern Matching
ambosSon0PM 0 0 = True
ambosSon0PM _ _ = False

esMultiploDe :: Int -> Int -> Bool
esMultiploDe x y = mod x y == 0

digitoUnidades :: Int -> Int
digitoUnidades x = mod x 10

digitoDecenas :: Int -> Int
digitoDecenas x = div (mod x 100) 10
