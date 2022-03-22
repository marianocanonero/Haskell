--Funciones Auxiliares

esPar :: Integer -> Bool
esPar n = mod n 2 == 0

esPrimo :: Integer -> Bool
esPrimo 1 = False
esPrimo n = n == menorDivisor n

menorDivisor :: Integer -> Integer
menorDivisor n = menorDivisorDesde n 2

menorDivisorDesde :: Integer -> Integer -> Integer
menorDivisorDesde n k
    | fromInteger k > sqrt (fromInteger n) = n 
    | mod n k == 0 = k
    | otherwise    = menorDivisorDesde n (k+1)

maximoPrimoHasta :: Integer -> Integer
maximoPrimoHasta i
    | esPrimo i = i
    | otherwise = maximoPrimoHasta (i-1)

--Satisface Goldbach-----------------------------

satisfaceGoldbach :: Integer -> Bool
satisfaceGoldbach n
    | n <= 2 && n > 0 = False
    | esPar n && esSumaDePrimos = True
    | n > 2 = False
    where 
        esSumaDePrimos = esSumaDePrimosMenoresQue n (n-1)

esSumaDePrimosMenoresQue :: Integer -> Integer -> Bool
esSumaDePrimosMenoresQue n i
    | i < 2 = False
    | esPrimo (n - maximoPrimoHasta i) = True
    | otherwise = esSumaDePrimosMenoresQue n (i-1)


--Verificar Conjetura Hasta Un Numero n----------

verificarConjeturaHasta :: Integer -> Bool
verificarConjeturaHasta n
    | n == 4 = True
    | satisfaceGoldbach n = satisfaceGoldbach (n-2)
    | n > 2 && esPar n = False

--Descomposicion en Primos-----------------------

descomposicionEnPrimos :: Integer -> (Integer,Integer)
descomposicionEnPrimos n
    | n > 2 && esPar n = (a,b)
    where
        a = valorDeA n (n-1)
        b = n - a


valorDeA :: Integer -> Integer -> Integer
valorDeA n i
    | esPrimo (n - maximoPrimoHasta i) = maximoPrimoHasta i
    | otherwise = valorDeA n (i-1)

--Numero de Descomposiciones---------------------

numeroDeDescomposiciones :: Integer -> Integer
numeroDeDescomposiciones n
    | n > 2 && esPar n = numeroDeDescomposicionAux n (n-1) 0 0

numeroDeDescomposicionAux :: Integer -> Integer -> Integer -> Integer -> Integer
numeroDeDescomposicionAux n i c d
    | i < 2 || a <= d = 0
    | a == b = 1
    | a == c = numeroDeDescomposicionAux n (i-1) a b
    | a /= b = 2 + numeroDeDescomposicionAux n (i-1) a b
    where
        a = valorDeA n i
        b = n - a




