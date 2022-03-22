type Posicion = [Int]

type Jugada = (Int, Int)

--Ejercicio 1

jugar :: Posicion -> Jugada -> Posicion
jugar [] _  = []
jugar posicion (pila,piedras)
    | pila == 1 = nuevaPosicion
    | otherwise = (pilaSeleccionada):(jugar (tail(posicion)) (pila-1,piedras))
    where
        pilaSeleccionada = head(posicion)
        pilaBuscada = pila
        nuevaPosicion = eliminarVacias((pilaSeleccionada - piedras):(tail(posicion)))

eliminarVacias :: Posicion -> Posicion
eliminarVacias [] = []
eliminarVacias (x:xs)
    | x == 0 = eliminarVacias xs
    | otherwise = x:(eliminarVacias xs)

--Ejercicio 2

posiblesJugadas :: Posicion -> [Jugada]
posiblesJugadas [] = []
posiblesJugadas posicion = (posiblesJugadasEnPila pila piedras) `concatenar` (posiblesJugadas (reverseList (tail(posicionRev))))
    where
        posicionRev = reverseList posicion
        piedras = head(posicionRev)
        pila = length(posicionRev)

reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

--Funcion auxiliar que dada una pila devuelve todas las posibles jugadas para la misma.
posiblesJugadasEnPila :: Int -> Int -> [Jugada]
posiblesJugadasEnPila pila 0 = []
posiblesJugadasEnPila pila piedras = (pila,piedras):(posiblesJugadasEnPila pila (piedras-1))

--Ejercicio 3

esPosicionGanadora :: Posicion -> Bool
esPosicionGanadora posicion = existeJugadaGanadora posicion (posiblesJugadas posicion)

existeJugadaGanadora :: Posicion -> [Jugada] -> Bool
existeJugadaGanadora posicion posiblesjugadas
    | posiblesjugadas == [] = False
    | otherwise = (existeJugadaGanadora posicion jugadasRestantes) || not (esPosicionGanadora jugada)
    where
        jugada = jugar posicion jugadaPosible
        jugadaPosible = head(posiblesjugadas)
        jugadasRestantes = tail(posiblesjugadas)

--Ejercicio 4 y 5

jugadaGanadora :: Posicion -> Jugada
jugadaGanadora posicion = head(buscaJugadasGanadoras posicion (posiblesJugadas posicion))

numeroDeJugadasGanadoras :: Posicion -> Int
numeroDeJugadasGanadoras [] = 0
numeroDeJugadasGanadoras posicion = longitud (buscaJugadasGanadoras posicion (posiblesJugadas posicion))

buscaJugadasGanadoras :: Posicion -> [Jugada] -> [Jugada]
buscaJugadasGanadoras posicion [] = []
buscaJugadasGanadoras posicion posiblesjugadas
    | not (esPosicionGanadora (jugada)) = jugadaPosible:(buscaJugadasGanadoras posicion jugadasRestantes)
    | otherwise = buscaJugadasGanadoras posicion jugadasRestantes
    where
        jugada = jugar posicion jugadaPosible
        jugadaPosible = head(posiblesjugadas)
        jugadasRestantes = tail(posiblesjugadas)

--Funciones Auxiliares

reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

concatenar :: [Jugada] -> [Jugada] -> [Jugada]
concatenar [] ys = ys
concatenar (x:xs) ys = concatenar xs (x:ys)

longitud :: [Jugada] -> Int
longitud [] = 0
longitud (x:xs) = (longitud xs) + 1



