-- Borja Aday Guadalupe Luis


-- Ejercicio 1

-- Aunque en el ejercicio se piden expresiones, haremos funciones igualadas a la expresión para poder
-- llamarlas desde el intérprete.

-- 1.a)
calculaAños :: Double
calculaAños = (10^10) / (365 * 24 * 60 * 60)

-- 1.b)
calculaTiempo :: [Char]
calculaTiempo =                   let años                       = div (10 ^ 10) (365 * 24 * 60 * 60)
                                      segundos_restantes_años    = (10 ^ 10) - (años * 365 * 24 * 60 * 60)
                                      dias                       = div segundos_restantes_años (24 * 60 * 60)
                                      segundos_restantes_dias    = segundos_restantes_años - (dias * 24 * 60 * 60)
                                      horas                      = div segundos_restantes_dias (60 * 60)
                                      segundos_restantes_horas   = segundos_restantes_dias - (horas * 60 * 60)
                                      minutos                    = div segundos_restantes_horas 60
                                      segundos                   = segundos_restantes_horas - (minutos * 60)
                                  in "Anios: " ++ show años ++ ", dias: " ++ show dias ++ ", horas: " ++ show horas ++ ", minutos: " ++ show minutos ++ ", segundos: " ++ show segundos

-- 1.c)
calculaTiempoGeneralizada :: Integer -> [Char]
calculaTiempoGeneralizada x =                   let años                       = div x (365 * 24 * 60 * 60)
                                                    segundos_restantes_años    = x - (años * 365 * 24 * 60 * 60)
                                                    dias                       = div segundos_restantes_años (24 * 60 * 60)
                                                    segundos_restantes_dias    = segundos_restantes_años - (dias * 24 * 60 * 60)
                                                    horas                      = div segundos_restantes_dias (60 * 60)
                                                    segundos_restantes_horas   = segundos_restantes_dias - (horas * 60 * 60)
                                                    minutos                    = div segundos_restantes_horas 60
                                                    segundos                   = segundos_restantes_horas - (minutos * 60)
                                                in "Anios: " ++ show años ++ ", dias: " ++ show dias ++ ", horas: " ++ show horas ++ ", minutos: " ++ show minutos ++ ", segundos: " ++ show segundos




-- Ejercicio 2

-- last [1..10^5] -> (0.01 secs, 7,255,128 bytes, 10^5 está dentro de los límites)

-- last [1..10^7] -> (0.19 secs, 720,056,560 bytes, 10^7 está dentro de los límites)

-- last [1..10^20] -> (No termina, debe generar la lista de tamaño 10^20)

-- head [1..10^20] -> (0.00 secs, 52,208 bytes, solo necesita generar la cabeza)

-- last [10^20..1] -> (Excepción porque debe expresarse como last [10^20,(10^20)-1..1], que tampoco terminaría)

-- head (tail [1..10^20]) -> (0.00 secs, 52,328 bytes, solo necesita generar el primer elemento de la lista [1..10^20])

-- length [1..10^20] -> (No termina, debe generar la lista de tamaño 10^20)

-- last (take (10^7) [1..10^20]) -> (0.21 secs, 1,280,057,744 bytes, solo necesita generar la lista de 10^7 que está dentro de los límites)

-- head (take (10^7) ([1..100] ++ [1..10^20])) -> (0.00 secs, 52,064 bytes, solo necesita generar el primer número)

-- last (take 100 ([1..10^20] ++ [1..100])) -> (0.00 secs, 72,040 bytes, solo necesita generar los 100 primeros números)

-- last (drop 100 ([1..10^20] ++ [1..100])) -> (No termina, debe calcular una lista de tamaño 10^20)

-- head (drop (10^7) ([1..10^20] ++ [1..100])) -> (0.22 secs, 1,280,058,000 bytes, necesita)

-- [1..10^7]==[1..10^7] -> (0.33 secs, 1,440,054,728 bytes, 10^7 está dentro de los límites)

-- [1..10^20]==[1..10^20] -> (No termina porque son iguales y debe comparar los 10^20 numeros)

-- [1..10^20]==[1..10^20+1] ->  (No termina porque a pesar de ser diferentes debe 
--                               comparar los 10^20 numeros primeros para encontrar la diferencia)

-- [1..10^20]==[2..10^20] -> (0.00 secs, 56,376 bytes, en este caso termina porque desde el primer número son diferentes)

-- head (reverse [1..10^7]) -> (1.19 secs, 960,056,608 bytes, debe generar una lista de 10^7 que está dentro de los límites)

-- last (reverse [1..10^7]) -> (1.06 secs, 960,051,792 bytes, debe generar una lista de 10^7 que está dentro de los límites)

-- reverse [1..10^20] == reverse [1..10^20+1] -> (No termina porque para hacer reverse necesita generar las listas)




-- Ejercicio 3

media :: Fractional a => [a] -> a
media xs = sum xs / fromIntegral (length xs)




-- Ejercicio 4

-- Digitos de un número entero X
digitos :: Integral a => a -> a
digitos x
    | x < 10 = 1
    | True = digitos (div x 10) + 1

-- Reducir entero sumando sus dígitos hasta que la suma sea menor de 10.
sumaDigitos :: Integral a => a -> a
sumaDigitos x 
    | x == 0 = 0
    | True = sumaDigitos (div x 10) + (mod x 10)

reduccion :: Integral a => a -> a
reduccion x
    | x < 0 = reduccion (-x)
    | (sumaDigitos x) < 10 = sumaDigitos x
    | (sumaDigitos x) >= 10 = reduccion (sumaDigitos x)

-- Numero de permutaciones de n elementos.
perm :: Integral a => a -> a
perm n
    | n == 0 = 1
    | n > 0 = n * perm (n - 1)
    | otherwise = error "Argumento negativo."

-- Numero de permutaciones de n elementos tomados de m en m.
var :: Integral a => a -> a -> a
var n m 
    | n >= 0 && m >= 0 = div (perm n) (perm (n - m))
    |otherwise = error "Ningún argumento puede ser negativo"

-- Numero de combinaciones de n elementos tomados de m en m.
comb :: Integral a => a -> a -> a
comb n m 
    | n >= 0 && m >= 0 = div (perm n) (perm m * perm (n - m))
    |otherwise = error "Ningún argumento puede ser negativo"




-- Ejercicio 5

-- Funcion estricta en el primer argumento.
and1 :: Bool -> Bool -> Bool
and1 False _ = False
and1 _ False = False
and1 True True = True

-- Funcion estricta en el segundo argumento.
and2 :: Bool -> Bool -> Bool
and2 _ False = False
and2 False _ = False
and2 True True = True

-- Funcion estricta en el primer argumento.
and3 :: Bool -> Bool -> Bool
and3 False _ = False
and3 True y = y

-- Funcion estricta en el segundo argumento.
and4 :: Bool -> Bool -> Bool
and4 _ False = False
and4 x True = x

-- Funcion estricta para ambos argumentos.
and5 :: Bool -> Bool -> Bool
and5 True y = y
and5 x True = x

-- Funcion estricta para ambos argumentos
and6 :: Bool -> Bool -> Bool
and6 True True = True
and6 False True = False
and6 True False = False
and6 False False = False