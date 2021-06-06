sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria l  = head l + sumatoria (tail l)

longitud :: [a] -> Int  -- Lo hago más genérico para el método 'zipi'
longitud [] = 0
longitud l  = 1 + longitud (tail l)

pertenece :: Int -> [Int] -> Bool
pertenece _ [] = False
pertenece x l  = (x == head l) || pertenece x (tail l)

primerMultiploDe45345 :: [Int] -> Int
primerMultiploDe45345 l
    | l == []                 = undefined   -- error('No contiene multiplos de 45345')?
    | mod (head l) 45345 == 0 = head l
    | otherwise               = primerMultiploDe45345 (tail l)

sumatoriaPM :: [Int] -> Int
sumatoriaPM []     = 0
sumatoriaPM (x:xs) = x + sumatoriaPM xs

longitudPM :: [Int] -> Int
longitudPM []     = 0
longitudPM (_:xs) = 1 + longitudPM xs

pertenecePM :: Int -> [Int] -> Bool
pertenecePM _ []    = False
pertenecePM x (h:t) = (x == h) || pertenecePM x t

productoria :: [Int] -> Int
productoria []    = 1
productoria (h:t) = h * productoria t

sumarN :: Int -> [Int] -> [Int]
sumarN _ []    = []
sumarN n (h:t) = (h + n) : (sumarN n t)

sumarElPrimero :: [Int] -> [Int]
sumarElPrimero (h:t) = sumarN h (h:t)

sumarElUltimo :: [Int] -> [Int]
sumarElUltimo l = sumarN (ultimo l) l

ultimo :: [Int] -> Int  -- Usar longitud?
ultimo (_:t)
    | tail t == [] = head t
    | otherwise    = ultimo t

pares :: [Int] -> [Int]
pares [] = []
pares (h:t)
    | mod h 2 == 0 = h:(pares t)
    | otherwise    = pares t

quitar :: Int -> [Int] -> [Int]
quitar _ [] = []
quitar x (h:t)
    | x == h    = t
    | otherwise = h:(quitar x t)

-- quitarUltimo?

quitarTodas :: Int -> [Int] -> [Int]
quitarTodas _ [] = []
quitarTodas x (h:t)
    | x == h    = (quitarTodas x t)
    | otherwise = h:(quitarTodas x t)

hayRepetidos :: [Int] -> Bool
hayRepetidos [] = False
hayRepetidos (h:t)
    | pertenece h t = True
    | otherwise     = hayRepetidos t

eliminarRepetidosAlFinal :: [Int] -> [Int]
eliminarRepetidosAlFinal [] = []
eliminarRepetidosAlFinal (h:t)
    | pertenece h t = h:(eliminarRepetidosAlFinal (quitarTodas h t))
    | otherwise     = h:(eliminarRepetidosAlFinal t)

eliminarRepetidosAlInicio :: [Int] -> [Int]
eliminarRepetidosAlInicio [] = []
eliminarRepetidosAlInicio (h:t)
    | pertenece h t = eliminarRepetidosAlInicio t
    | otherwise     = h:(eliminarRepetidosAlInicio t)

maximo :: [Int] -> Int
maximo []    = minBound         -- Supuestamente da el valor mínimo posible de Int
maximo (h:t) = max h (maximo t)

minimo :: [Int] -> Int
minimo []    = maxBound
minimo (h:t) = min h (minimo t)

ordenar :: [Int] -> [Int]
ordenar [] = []
ordenar l = (minimo l):(ordenar (quitar (minimo l) l))

reverso :: [Int] -> [Int]
reverso l
    | longitud l > 1 = (ultimo l):(reverso (quitar (ultimo l) l))
    | otherwise      = (head l):[]

concatenar :: [Int] -> [Int] -> [Int]
concatenar l1 l2
    | l1 == []  = l2
    | otherwise = (head l1):(concatenar (tail l1) l2)

zipi :: [a] -> [b] -> [(a,b)]
zipi l1 l2
    -- | (longitud l1) > (longitud l2) = zipi' l2 l1    -- Idea que no funcionó por los tipos de datos
    | (longitud l1 == 1) || (longitud l2 == 1) = ((head l1), (head l2)):[]
    -- | ((tail l2) == []) = ((head l1), (head l2)):[]  -- Por qué tiraba error??
    | otherwise                              = ((head l1), (head l2)):(zipi (tail l1) (tail l2))
