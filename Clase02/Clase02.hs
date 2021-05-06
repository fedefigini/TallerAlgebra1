estanRelacionados :: Float -> Float -> Bool
estanRelacionados x y
    | x <= 3 && y <= 3                       = True
    | (x > 3 && x <= 7) && (y > 3 && y <= 7) = True
    | x > 7 && y > 7                         = True
    | otherwise                              = False

prodInt :: (Float, Float) -> (Float, Float) -> Float
prodInt (vx, vy) (wx, wy) = vx * wx + vy * wy
-- prodInt x y = fst(x) * fst(y) + snd(x) * snd(y)  -- Otra opción, fst y snd nativos de Haskell

todoMenor :: (Float, Float) -> (Float, Float) -> Bool
todoMenor x y = fst(x) < fst(y) && snd(x) < snd(y)

distanciaPuntos :: (Float, Float) -> (Float, Float) -> Float
distanciaPuntos (vx, vy) (wx, wy) = sqrt ((vx - wx)^2 + (vy - wy)^2)

sumaTerna :: (Int, Int, Int) -> Int
sumaTerna (x, y, z) = x + y + z

posicPrimerPar :: (Int, Int, Int) -> Int
posicPrimerPar (x, y, z)
    | mod x 2 == 0 = 1 -- Arranco en 1?
    | mod y 2 == 0 = 2
    | mod z 2 == 0 = 3
    | otherwise    = 4

crearPar :: a -> b -> (a, b)
crearPar x y = (x, y)

invertir :: (a, b) -> (b, a)
invertir (x, y) = (y, x)
