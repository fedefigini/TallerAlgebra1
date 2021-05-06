satisfaceGoldbach :: Integer -> Bool
satisfaceGoldbach n
    | n <= 2       = undefined                                      -- Undefined me parece lo más adecuado
    | mod n 2 /= 0 = undefined                                      -- Pero podría perfectamente reemplazarse por False
    | otherwise    = fst (calcularGoldbachIterativo 2 (div n 2) n)  -- Alcanza con iterar hasta n/2 y son números pares

-- Explicación: Dado que vienen de a pares, si i > n/2 lo cumple entonces n-i lo cumple también y n-i < n/2

calcularGoldbachIterativo :: Integer -> Integer -> Integer -> (Bool, Maybe (Integer, Integer))
calcularGoldbachIterativo i end n
    | i > end   = (False, Nothing)                                  -- Itero hasta el valor deseado (OJO! n/2 incluído)
    | satisface = (True, Just (i, n-i))                             -- Chequeo si lo satisface
    | otherwise = calcularGoldbachIterativo (i+1) end n             -- Si no, paso al siguiente índice
    where satisface = esTuplaPrima (i, n-i)                         -- Necesito el n acá -> Memoria VS Tiempo

-- Explicación: Podría pasar sólo n o sólo (div n 2) y calcular uno a partir del otro
--              Pero eso agregaría más cálculos por cada iteración lo cual sumaría tiempo de ejecución (asumo)

esTuplaPrima :: (Integer, Integer) -> Bool
esTuplaPrima (x, y) = esPrimo x && esPrimo y                        -- Chequeo si n es suma de dos primos

esPrimo :: Integer -> Bool
esPrimo n
    | n < 2     = False                                             -- 2 es el primer primo
    | n == 2    = True                                              -- Me evita problemas con la raíz
    | otherwise = esPrimoIterativo 2 (raizEntera n) n               -- Alcanza con iterar hasta sqrt(n)

-- Explicación: Si i | n / i > sqrt(n) => i = k*n && k < sqrt(n) => k | n && k < sqrt(n)
--              O k = 1 => i = n => Primo y trivial

esPrimoIterativo :: Integer -> Integer -> Integer -> Bool
esPrimoIterativo i end n
    | i > end      = True                                           -- Itero hasta el valor deseado
    | mod n i == 0 = False                                          -- Si es divisible por i < n, no es primo
    | otherwise    = esPrimoIterativo (i+1) end n                   -- Si no, paso al siguiente índice*

-- *: Se podrían hacer dos guardas extra para i == 2 (i|n o no) y luego aumentar i+2 pero no cambia en O(n), preferencias?
--    Testeándolo un poco no noté gran diferencia aunque debería ser n VS n/2?

raizEntera :: Integer -> Integer
raizEntera n = ceiling (sqrt (fromIntegral n))                      -- Tomo el mínimo valor entero superior a la raíz

verificarConjeturaHasta :: Integer -> Bool
verificarConjeturaHasta n
    | n <= 2       = undefined                                      -- Undefined me parece lo más adecuado
    | mod n 2 /= 0 = undefined                                      -- Pero podría perfectamente reemplazarse por False
    | otherwise    = iterarConjetura 4 n                            -- Itero de 4 a n, por cada par

iterarConjetura :: Integer -> Integer -> Bool                       -- Podría juntarse con iterarDescomposiciones por Tupla
iterarConjetura i end
    | i > end             = True                                    -- Itero hasta el valor deseado
    | satisfaceGoldbach i = iterarConjetura (i+2) end               -- Mientras lo satisfaga, aumento el índice (+2 por pares)
    | otherwise           = False                                   -- Si no lo satisface, gané 1 millón de dólares (?

descomposicionEnPrimos :: Integer -> Maybe (Integer, Integer)
descomposicionEnPrimos n
    | n <= 2       = undefined                                      -- Undefined me parece lo más adecuado
    | mod n 2 /= 0 = undefined                                      -- Pero podría perfectamente reemplazarse por False
    | otherwise    = snd (calcularGoldbachIterativo 2 (div n 2) n)  -- Alcanza con iterar hasta n/2 y son números pares

numeroDeDescomposiciones :: Integer -> Integer
numeroDeDescomposiciones n
    | n <= 2       = undefined                                      -- Undefined me parece lo más adecuado
    | mod n 2 /= 0 = undefined                                      -- Pero podría perfectamente reemplazarse por False
    | otherwise    = iterarDescomposiciones 2 0 (div n 2) n         -- Itero sobre todos los enteros desde 2 a n/2

iterarDescomposiciones :: Integer -> Integer -> Integer -> Integer -> Integer
iterarDescomposiciones i count end n
    | i == end && satisface = count + 1                                         -- Si el n/2 lo cumple, sumo 1 (no es un par)
    | i == end              = count                                             -- Si no lo cumple, devuelvo la cuenta tal cual
    | satisface             = iterarDescomposiciones (i+1) (count+2) end n      -- Sumo dos por el par (i, n-i) y (n-i, i)
    | otherwise             = iterarDescomposiciones (i+1) count end n          -- Si no lo cumple, sigo aumentando i
    where satisface = esTuplaPrima (i, n-i)                                     -- Para achicar el largo nomás

{-
#### Más optimizado? #### -> Mismo * de casos especiales para i = 2 y luego iterar de a impares
iterarDescomposiciones :: Integer -> Integer -> Integer -> Integer -> Integer
iterarDescomposiciones i count end n
    | i > end               = count                                             -- Itero hasta superar el n/2 (Entro si n/2 par)
    | i == end && satisface = count + 1                                         -- Si el n/2 lo cumple, sumo 1 (Caso n/2 impar)
    | i == end              = count                                             -- Si no lo cumple, devuelvo la cuenta tal cual
    | i == 2 && satisface   = iterarDescomposiciones (i+1) (count+2) end n      -- Caso especial por ser el único primo par
    | i == 2                = iterarDescomposiciones (i+1) count end n          -- En el resto sumo i+2 para ir por impares
    | satisface             = iterarDescomposiciones (i+2) (count+2) end n      -- Sumo dos a count por (i, n-i) y (n-i, i)
    | otherwise             = iterarDescomposiciones (i+2) count end n          -- Si no lo cumple, sigo aumentando i
    where satisface = esPrimo i && esPrimo (n-i)                                -- Para achicar el largo nomás
-}

{-
#### Reemplazada por fst (calculacalcularGoldbachIterativo) ####
checkearGoldbachIterativo :: Integer -> Integer -> Integer -> Bool
checkearGoldbachIterativo i end n
    | i > end   = False                                         -- Itero hasta el valor deseado (OJO! n/2 incluído)
    | satisface = True                                          -- Chequeo si lo satisface
    | otherwise = checkearGoldbachIterativo (i+1) end n         -- Si no, paso al siguiente índice
    where satisface = esPrimo i && esPrimo (n-i)                -- Necesito el n acá -> Memoria VS Tiempo

#### Reemplazada por snd (calculacalcularGoldbachIterativo) ####
calcularGoldbachIterativo :: Integer -> Integer -> Integer -> (Integer, Integer)
calcularGoldbachIterativo i end n                               -- Muy similar a checkearGoldbachIterativo
    | i > end   = error "($.$)"                                 -- Itero hasta el valor deseado (OJO! n/2 incluído)
    | satisface = (i, n-i)                                      -- Chequeo si lo satisface
    | otherwise = calcularGoldbachIterativo (i+1) end n         -- Si no, paso al siguiente índice
    where satisface = esPrimo i && esPrimo (n-i)                -- Necesito el n acá -> Memoria VS Tiempo
-}

-- FUNCIONES PARA DEBUGGEAR/TESTEAR --

chequearConsigna :: [(Integer, Integer)]
chequearConsigna = (88, numeroDeDescomposiciones 88):(10, numeroDeDescomposiciones 10):(4, numeroDeDescomposiciones 4):[(123456, numeroDeDescomposiciones 123456)]

numDescomposicionesList :: Integer -> [(Integer, Integer)]
numDescomposicionesList 2 = []
numDescomposicionesList n = (n, numeroDeDescomposiciones n):(numDescomposicionesList (n-2))

descomposicionesList :: Integer -> [(Integer, Maybe (Integer, Integer))]
descomposicionesList 2 = []
descomposicionesList n = (n, descomposicionEnPrimos n):(descomposicionesList (n-2))

nEsimosPrimos :: Integer -> [Integer]
nEsimosPrimos 0 = []
nEsimosPrimos n = (nEsimoPrimo n):(nEsimosPrimos (n-1))

nEsimoPrimo :: Integer -> Integer
nEsimoPrimo 0 = 1
nEsimoPrimo n = minimoPrimoDesde (1 + nEsimoPrimo (n-1))

minimoPrimoDesde :: Integer -> Integer
minimoPrimoDesde n
    | esPrimo n = n
    | otherwise = minimoPrimoDesde (n+1)
