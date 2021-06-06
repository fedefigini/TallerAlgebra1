type Set a = [a]

vacio :: Set Int
vacio = []

pertenece :: Int -> Set Int -> Bool
pertenece _ [] = False
pertenece x (h:t)
    | x == h    = True
    | otherwise = pertenece x t

agregar :: Int -> Set Int -> Set Int
agregar x c
    | pertenece x c = c
    | otherwise     = x:c

cardinal :: Set Int -> Int
cardinal [] = 0
cardinal (h:t)
    | pertenece h t = cardinal t
    | otherwise     = 1 + cardinal t

incluido :: Set Int -> Set Int -> Bool
incluido [] c = True
incluido (x:xs) c = (pertenece x c) && (incluido xs c)

iguales :: Set Int -> Set Int -> Bool
iguales c1 c2 = (incluido c1 c2) && (incluido c2 c1)

union :: Set Int -> Set Int -> Set Int
union [] c = c
union (h:t) c
    | pertenece h c = union t c
    | otherwise     = union t (agregar h c)

interseccion :: Set Int -> Set Int -> Set Int
interseccion [] c = []
interseccion (h:t) c
    | pertenece h c = agregar h (interseccion t c)
    | otherwise     = interseccion t c

diferencia :: Set Int -> Set Int -> Set Int
diferencia [] c = []
diferencia (h:t) c
    | pertenece h c = diferencia t c
    | otherwise     = agregar h (diferencia t c)

diferenciaSimetrica :: Set Int -> Set Int -> Set Int
diferenciaSimetrica a b = diferencia (union a b) (interseccion a b)

partes :: Set Int -> Set (Set Int)
partes [] = [[]]
partes (h:t) = unionC (partes t) (agregarATodos h (partes t))

agregarATodos :: Int -> Set (Set Int) -> Set (Set Int)
agregarATodos x [] = []
agregarATodos x (h:t) = agregarC (agregar x h) (agregarATodos x t)

agregarC :: Set Int -> Set (Set Int) -> Set (Set Int)
agregarC x c
    | perteneceC x c = c
    | otherwise     = x:c

perteneceC :: Set Int -> Set (Set Int) -> Bool
perteneceC x [] = False
perteneceC x (h:t)
    | iguales x h = True
    | otherwise   = perteneceC x t

unionC :: Set (Set Int) -> Set (Set Int) -> Set (Set Int)
unionC [] c = c
unionC (h:t) c
    | perteneceC h c = unionC t c
    | otherwise     = unionC t (agregarC h c)

partesN :: Int -> Set (Set Int)
partesN 0 = [[]]
partesN n = unionC (partesN (n-1)) (agregarATodos n (partesN (n-1)))

{-
[1,2,3] * [3,4]

[(1,3),(2,3),(3,3),(1,4),(2,4),(3,4)]

 [(1,3),(1,4)] -> [1] * [3,4]
U[(2,3),(2,4)] -> [2] * [3,4]
U[(3,3),(3,4)] -> [3] * [3,4]
-}

productoCartesiano :: Set Int -> Set Int -> Set (Int, Int)
productoCartesiano _ []    = []
productoCartesiano [] _    = []
productoCartesiano (h:t) c = unionT (tuplas h c) (productoCartesiano t c)

tuplas :: Int -> Set Int -> Set (Int, Int)
tuplas _ []    = []
tuplas x (h:t) = agregarT (x,h) (tuplas x t)

agregarT :: (Int, Int) -> Set (Int, Int) -> Set (Int, Int)
agregarT t c
    | perteneceT t c = c
    | otherwise      = t:c

perteneceT :: (Int, Int) -> Set (Int, Int) -> Bool
perteneceT _ [] = False
perteneceT x (h:t)
    | igualesT x h = True
    | otherwise    = perteneceT x t

igualesT :: (Int, Int) -> (Int, Int) -> Bool
igualesT (ux, uy) (vx, vy) = (ux == vx) && (uy == vy)

unionT :: Set (Int, Int) -> Set (Int, Int) -> Set (Int, Int)
unionT [] c = c
unionT (h:t) c
    | perteneceT h c = unionT t c
    | otherwise      = unionT t (agregarT h c)

{-
tuplas :: Int -> Set Int -> Set (Int, Int)
tuplas _ [] = []
tuplas x (h:t)  = agregarT (x,h) t

agregarT :: (Int, Int) -> Set (Int, Int) -> Set(Int, Int)
agregarT _ [] = []
agregarT x (h:t) = x:(agregarT x t)

unionT :: (Int, Int) -> Set (Int, Int) -> Set (Int, Int) -> Set (Int, Int)
unionT t a b = agregar (agregarC t a) b
-}