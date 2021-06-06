--------------------------------------------------------------------------------
fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n-1)

type Set a = [a]

vacio :: Set a
vacio = []

agregar :: Eq a => a -> Set a -> Set a
agregar n c | n `elem` c = c
            | otherwise = n:c

union :: Eq a => Set a -> Set a -> Set a
union [] ys     = ys
union (x:xs) ys = union xs (agregar x ys)

--------------------------------------------------------------------------------
---------------------------- Funciones Clase 8 ---------------------------------
--------------------------------------------------------------------------------

combinatorio :: Int -> Int -> Int
combinatorio n k = div (fact n) ((fact k) * fact(n-k))

combinatorioR :: Int -> Int -> Int
combinatorioR n 0 = 1
combinatorioR n k
    | n == k    = 1
    | otherwise = (combinatorioR (n-1) k) + (combinatorioR (n-1) (k-1))

variaciones :: Set Int -> Int -> Set [Int]
variaciones _ 0 = [[]]
variaciones c n = agregarElementosAListas c (variaciones c (n-1))

agregarElementosAListas :: Set Int -> Set [Int] -> Set [Int]
agregarElementosAListas [] _    = []
agregarElementosAListas (h:t) c = union (agregarElementoAdelante h c) (agregarElementosAListas t c)

agregarElementoAdelante :: Int -> Set [Int] -> Set [Int]
agregarElementoAdelante _ []    = []
agregarElementoAdelante x (h:t) = agregar (x:h) (agregarElementoAdelante x t)

insertarEn :: [Int] -> Int -> Int -> [Int]
insertarEn l n 1 = n:l
insertarEn l n i = (head l):(insertarEn (tail l) n (i-1))

permutaciones :: Set Int -> Set [Int]
permutaciones []    = [[]]
permutaciones (h:t) = insertarEnCadaPosDeTodasLasListas (permutaciones t) h

insertarEnCadaPosDeTodasLasListas :: Set [Int] -> Int -> Set [Int]
insertarEnCadaPosDeTodasLasListas [] n    = []
insertarEnCadaPosDeTodasLasListas (h:t) n = union (insertarEnCadaPos h n ((length h) + 1)) (insertarEnCadaPosDeTodasLasListas t n)

insertarEnCadaPos :: [Int] -> Int -> Int -> Set [Int]
insertarEnCadaPos c n 1 = agregar (insertarEn c n 1) vacio
insertarEnCadaPos c n i = agregar (insertarEn c n i) (insertarEnCadaPos c n (i-1))
