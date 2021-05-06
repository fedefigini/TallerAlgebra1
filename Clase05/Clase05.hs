fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n-1)

prod :: Int -> Int -> Int
prod d h
    | d == h    = d
    | otherwise = h * prod d (h-1)

fact' :: Int -> Int
fact' n = prod 1 n

prod' :: Int -> Int -> Int
prod' d h
    | d == h    = h
    | otherwise = d * prod' (d+1) h

sumaDivisores :: Int -> Int
-- sumaDivisores n = sumaDivisoresHasta n n             -- Equivalentes
sumaDivisores n = sumaDivisoresDesde n 1                -- Sólo distinto caso base

sumaDivisoresHasta :: Int -> Int -> Int
sumaDivisoresHasta n k
    | k == 1       = 1
    | mod n k == 0 = k + sumaDivisoresHasta n (k-1)
    | otherwise    = sumaDivisoresHasta n (k-1)

sumaDivisoresDesde :: Int -> Int -> Int
sumaDivisoresDesde n k
    | k == n       = n
    | mod n k == 0 = k + sumaDivisoresDesde n (k+1)
    | otherwise    = sumaDivisoresDesde n (k+1)

menorDivisor :: Int -> Int
menorDivisor n = menorDivisorDesde n 2

menorDivisorDesde :: Int -> Int -> Int
menorDivisorDesde n k
    | mod n k == 0 = k
    | otherwise    = menorDivisorDesde n (k+1)

esPrimo :: Int -> Bool
esPrimo 1 = False
esPrimo n = menorDivisor n == n

esPrimo' :: Int -> Bool
esPrimo' n = (n > 1) && not (tieneDivisoresDesde n 2)

tieneDivisoresDesde :: Int -> Int -> Bool       -- verdadero si tiene divisores en el intervalo [k, n)
tieneDivisoresDesde n k
    | k == n    = False
    | otherwise = mod n k == 0 || tieneDivisoresDesde n (k+1)

nEsimoPrimo :: Int -> Int
nEsimoPrimo 1 = 2
nEsimoPrimo n = minimoPrimoDesde (1 + nEsimoPrimo (n-1))

minimoPrimoDesde :: Int -> Int
minimoPrimoDesde n
    | esPrimo n = n
    | otherwise = minimoPrimoDesde (n+1)

menorFactDesde :: Int -> Int
menorFactDesde m = menorFactDesdeDesde 1 m

menorFactDesdeDesde :: Int -> Int -> Int
menorFactDesdeDesde i m
    | (fact i) >= m = fact i
    | otherwise     = menorFactDesdeDesde (i+1) m

mayorFactHasta :: Int -> Int
mayorFactHasta m = mayorFactHastaDesde 1 m

mayorFactHastaDesde :: Int -> Int -> Int
mayorFactHastaDesde i m
    | fact i == m = fact i
    | fact i >= m = fact (i-1)
    | otherwise   = mayorFactHastaDesde (i+1) m

esFact :: Int -> Bool
esFact n = mayorFactHasta n == menorFactDesde n

esFibonacci :: Int -> Bool
esFibonacci n = menorFibonacciDesde n == mayorFibonacciHasta n

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

menorFibonacciDesde :: Int -> Int
menorFibonacciDesde n = menorFibonacciDesdeDesde 1 n

menorFibonacciDesdeDesde :: Int -> Int -> Int
menorFibonacciDesdeDesde i n
    | fibonacci i >= n = fibonacci i
    | otherwise        = menorFibonacciDesdeDesde (i+1) n

mayorFibonacciHasta :: Int -> Int
mayorFibonacciHasta n = mayorFibonacciHastaDesde 1 n

mayorFibonacciHastaDesde :: Int -> Int -> Int
mayorFibonacciHastaDesde i n
    | fibonacci i == n = fibonacci i
    | fibonacci i > n  = fibonacci (i-1)
    | otherwise        = mayorFibonacciHastaDesde (i+1) n

esSumaInicialDePrimos :: Int -> Bool
esSumaInicialDePrimos n = menorSumaInicialDePrimosDesde n == mayorSumaInicialDePrimosHasta n

sumaInicialDePrimos :: Int -> Int
sumaInicialDePrimos n = sumaInicialDePrimosDesde 1 n

sumaInicialDePrimosDesde :: Int -> Int -> Int
sumaInicialDePrimosDesde i n
    | i == n && esPrimo n = n
    | i == n              = 0                                       -- not (esPrimo n) implícito
    | esPrimo i           = i + sumaInicialDePrimosDesde (i+1) n
    | otherwise           = sumaInicialDePrimosDesde (i+1) n

menorSumaInicialDePrimosDesde :: Int -> Int
menorSumaInicialDePrimosDesde n = menorSumaInicialDePrimosDesdeDesde 1 n

menorSumaInicialDePrimosDesdeDesde :: Int -> Int -> Int
menorSumaInicialDePrimosDesdeDesde i n
    | sumaInicialDePrimos i >= n = sumaInicialDePrimos i
    | otherwise                  = menorSumaInicialDePrimosDesdeDesde (i+1) n

mayorSumaInicialDePrimosHasta :: Int -> Int
mayorSumaInicialDePrimosHasta n = mayorSumaInicialDePrimosHastaDesde 1 n

mayorSumaInicialDePrimosHastaDesde :: Int -> Int -> Int
mayorSumaInicialDePrimosHastaDesde i n
    | sumaInicialDePrimos i == n = sumaInicialDePrimos i
    | sumaInicialDePrimos i > n  = sumaInicialDePrimos (i-1)
    | otherwise                  = mayorSumaInicialDePrimosHastaDesde (i+1) n

tomaValorMax :: Int -> Int -> Int
tomaValorMax n1 n2 = getValorMax n1 n1 n2

getValorMax :: Int -> Int -> Int -> Int
getValorMax i max end                                   -- i: índice iterador, max: máximo hasta índice i, end: último índice
    | i > end   = sumaDivisores max
    | isBigger  = getValorMax (i+1) i end
    | otherwise = getValorMax (i+1) max end
    where isBigger = sumaDivisores i > sumaDivisores max

tomaValorMin :: Int -> Int -> Int
tomaValorMin n1 n2 = getValorMin n1 n1 n2

getValorMin :: Int -> Int -> Int -> Int
getValorMin i min end
    | i > end   = sumaDivisores min
    | isSmaller = getValorMin (i+1) i end
    | otherwise = getValorMin (i+1) min end
    where isSmaller = sumaDivisores i < sumaDivisores min

primosGem :: Int -> Int
primosGem n 
    | n < 5     = 0                                     -- El primer caso es (3,5) por lo que arranco ahí
    | otherwise = getPrimosGem 5 0 n

getPrimosGem :: Int -> Int -> Int -> Int
getPrimosGem i count end                                -- i: iterador, count: acumulador, end: último índice
    | i > end = count
    | isPrimosGem = getPrimosGem (i+2) (count+1) end    -- sumo de a 2 dado que los pares no van a ser primos
    | otherwise   = getPrimosGem (i+2) count end
    where isPrimosGem = esPrimo i && esPrimo (i-2)

proxPrimosGem :: Int -> (Int, Int)
proxPrimosGem n 
    | mod n 2 /= 0 = proxPrimosGem (n+1)                -- Si es impar le sumo uno
    | isPrimosGem  = (n+1, n+3)
    | otherwise    = proxPrimosGem (n+2)
    where isPrimosGem = esPrimo (n+1) && esPrimo (n+3)    -- Porque checkeo n+1 y n+3

largoSecuencia :: Int -> Int
largoSecuencia n = lotharCollatzCount n 0

lotharCollatzCount :: Int -> Int -> Int
lotharCollatzCount n count
    | n == 1       = count
    | mod n 2 == 0 = lotharCollatzCount (div n 2) (count+1)
    | otherwise    = lotharCollatzCount (3*n + 1) (count+1)

maxLargoSecuencia :: Int -> Int
maxLargoSecuencia n = getMaxLargoSecuencia 1 1 n

getMaxLargoSecuencia :: Int -> Int -> Int -> Int
getMaxLargoSecuencia i max end
    | i > end   = max
    | isBigger  = getMaxLargoSecuencia (i+1) i end
    | otherwise = getMaxLargoSecuencia (i+1) max end
    where isBigger = largoSecuencia i > largoSecuencia max
