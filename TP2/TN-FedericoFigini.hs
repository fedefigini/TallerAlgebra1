type Posicion = [Int]       -- No importa el orden de los montones, es un Set

type Jugada = (Int, Int)    -- (x,y) / x: Pila (índice empieza en 1), y: Cantidad de piedras a retirar

-------- Ejercicio 1 --------

jugar :: Posicion -> Jugada -> Posicion     -- No chequeo que la jugada sea válida porque lo garantiza la consigna
jugar (h:t) (1,n)                           -- Si llegué a la pila deseada, debo modificarla
    | h == n    = t                         -- Si saco todas las piedras, debo borrar esa pila
    | otherwise = (h-n):t                   -- Si no, dejo la cantidad de piedras restantes en la pila
jugar (h:t) (i,n) = h:(jugar t ((i-1),n))   -- Si no es el índice deseado, itero hasta llegar al mismo

-------- Ejercicio 2 --------

posiblesJugadas :: Posicion -> [Jugada]
posiblesJugadas p = posiblesJugadasConIndice p 1    -- Necesito pasarle el índice de la pila para crear las tuplas

posiblesJugadasConIndice :: Posicion -> Int -> [Jugada]
posiblesJugadasConIndice [] _    = []                                                           -- Si ya vi todas las pilas terminé
posiblesJugadasConIndice (h:t) i = (jugadasPorPila i h)++(posiblesJugadasConIndice t (i+1))     -- Si no, creo las tuplas por pila

jugadasPorPila :: Int -> Int -> [Jugada]
jugadasPorPila _ 0 = []                                 -- Si llegué al 0 en n ya hice todas las tuplas posibles
jugadasPorPila i n = (i,n):(jugadasPorPila i (n-1))     -- Creo las tuplas (pila, piedrasARetirar)

-------- Ejercicio 3 --------

expandirJugadas :: Posicion -> [(Jugada, Posicion)]         -- Devuelve una lista de tuplas (jugadaPosible, posicionResultante)
expandirJugadas [] = []                                     -- Si no tengo pilas no tengo más jugadas
expandirJugadas p  = jugarTodas p (posiblesJugadas p)       -- Si no, hago cada jugada posible

jugarTodas :: Posicion -> [Jugada] -> [(Jugada, Posicion)]
jugarTodas _ []    = []                                     -- Si no me quedan jugadas, terminé
jugarTodas p (h:t) = (h, (jugar p h)):(jugarTodas p t)      -- Si no, agrego la tupla (jugada, posicionResulante)

esPosicionGanadora :: Posicion -> Bool
esPosicionGanadora [] = False                                   -- Si no me quedan movimientos, perdí
esPosicionGanadora p  = algunaEsPerdedora (expandirJugadas p)   -- Si no, veo si alguna jugada mía lo fuerza a perder

algunaEsPerdedora :: [(Jugada, Posicion)] -> Bool
algunaEsPerdedora []    = False                                                         -- Si se quedó sin movimientos, perdió
algunaEsPerdedora (h:t) = (not (esPosicionGanadora (snd h))) || (algunaEsPerdedora t)   -- Si no, veo si alguna de sus jugadas no es ganadora

-------- Ejercicio 4 --------

jugadaGanadora :: Posicion -> Jugada                    -- No chequeo que la posición sea ganadora porque lo asegura la consigna
jugadaGanadora p = buscarGanadora (expandirJugadas p)   -- Busco la jugada ganadora iterando sobre las jugadas posibles

buscarGanadora :: [(Jugada, Posicion)] -> Jugada
buscarGanadora (h:t)
    | not (esPosicionGanadora (snd h)) = fst h              -- Si es una jugada perdedora para mi oponente, es ganadora para mí
    | otherwise                        = buscarGanadora t   -- Si no, veo si lo cumple la siguiente

-------- Ejercicio 5 --------

numeroDeJugadasGanadoras :: Posicion -> Int
numeroDeJugadasGanadoras [] = 0                                     -- Si no quedan piedras no tengo ninguna jugada, menos ganadora, ya perdí
numeroDeJugadasGanadoras p  = contarGanadoras (expandirJugadas p)   -- Si no, cuento las jugadas ganadoras sobre las posibles

contarGanadoras :: [(Jugada, Posicion)] -> Int
contarGanadoras [] = 0                                              -- Si no me quedan jugadas, ninguna (más) va a serlo
contarGanadoras (h:t)
    | not (esPosicionGanadora (snd h)) = 1 + contarGanadoras t      -- Si es perdedora para mi oponente es ganadora para mí, por lo que sumo 1 al contador
    | otherwise                        = contarGanadoras t          -- Si no lo es, reviso las siguientes sin aumentar el contador

-------- Testeo --------
test :: Bool
test = (posiblesJugadas [] == [])
    && (posiblesJugadas [1] == [(1,1)])
    && (posiblesJugadas [1,2,2] == [(1,1),(2,2),(2,1),(3,2),(3,1)])
    && (esPosicionGanadora [] == False)
    && (esPosicionGanadora [1] == True)
    && (esPosicionGanadora [1,2] == True)
    && (esPosicionGanadora [1,2,3] == False)
    && (esPosicionGanadora [1,2,3,4] == True)
    && (esPosicionGanadora [1,1] == False)
    && (esPosicionGanadora [1,2,2] == True)
    && (jugar [3,3,3] (1,3) == [3,3])
    && (jugar [3,3,3] (2,1) == [3,2,3])
    && (jugar [5,4,3,2,1] (2,3) == [5,1,3,2,1])
    && (jugadaGanadora [1,2,3,4] == (4,4))
    && (jugadaGanadora [1,1,1] == (1,1) || jugadaGanadora [1,1,1] == (2,1) || jugadaGanadora [1,1,1] == (3,1))
    && (numeroDeJugadasGanadoras [] == 0)
    && (numeroDeJugadasGanadoras [1] == 1)
    && (numeroDeJugadasGanadoras [1,2,3,4] == 1)
    && (numeroDeJugadasGanadoras [1,1,1] == 3)
