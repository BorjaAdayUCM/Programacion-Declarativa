-- Ejercicio 1
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use guards" #-}
{-# HLINT ignore "Use infix" #-}

a11:: Int
a11 = 1194 * 10^15 * 10^6 * 13700 * 10^6 * 365 * 24 * 60 * 60

a12:: Integer
a12 = 1194 * 10^15 * 10^6 * 13700 * 10^6 * 365 * 24 * 60 * 60

a13:: Double
a13 = 1194 * 10^15 * 10^6 * 13700 * 10^6 * 365 * 24 * 60 * 60

b1:: Integer
b1 = 10^10 `div` (365 * 24 * 60^2)

c1 :: [Char]
c1 =                            let años                       = div (10 ^ 10) (365 * 24 * 60 * 60)
                                    segundos_restantes_años    = (10 ^ 10) - (años * 365 * 24 * 60 * 60)
                                    dias                       = div segundos_restantes_años (24 * 60 * 60)
                                    segundos_restantes_dias    = segundos_restantes_años - (dias * 24 * 60 * 60)
                                    horas                      = div segundos_restantes_dias (60 * 60)
                                    segundos_restantes_horas   = segundos_restantes_dias - (horas * 60 * 60)
                                    minutos                    = div segundos_restantes_horas 60
                                    segundos                   = segundos_restantes_horas - (minutos * 60)
                                in "Anios: " ++ show años ++ ", dias: " ++ show dias ++ ", horas: " ++ show horas ++ ", minutos: " ++ show minutos ++ ", segundos: " ++ show segundos

d1:: Int -> Int
d1 seg = seg `div` (365 * 24 * 60^2)

d2:: Int -> [Char]
d2 seg =                        let años                       = div seg (365 * 24 * 60 * 60)
                                    segundos_restantes_años    = seg - (años * 365 * 24 * 60 * 60)
                                    dias                       = div segundos_restantes_años (24 * 60 * 60)
                                    segundos_restantes_dias    = segundos_restantes_años - (dias * 24 * 60 * 60)
                                    horas                      = div segundos_restantes_dias (60 * 60)
                                    segundos_restantes_horas   = segundos_restantes_dias - (horas * 60 * 60)
                                    minutos                    = div segundos_restantes_horas 60
                                    segundos                   = segundos_restantes_horas - (minutos * 60)
                                in "Anios: " ++ show años ++ ", dias: " ++ show dias ++ ", horas: " ++ show horas ++ ", minutos: " ++ show minutos ++ ", segundos: " ++ show segundos

-- Ejercicio 2

f1:: Int -> Int -> Int
f1 x y = 2*x - y*x

g1:: Int -> Int
g1 x = f1 (f1 2 x) (f1 x 1)

h1:: Int -> Int -> Int -> Int
h1 x y z = f1 (f1 (x+2*y) (g1 3)) (5 - g1 z - y)

i1:: Int -> Int -> Int
i1 x y
   | x >= y && y > 0 = x - y
   | 0 < x && x < y = 0
   | otherwise = y - x

f2:: Int -> Int -> Int
f2 x y = (-) ((*) 2 x) ((*) y x)

h2:: Int -> Int -> Int -> Int
h2 x y z = f1 (f1 ((+) x ((*) 2 y)) (g1 3)) ((-) 5 ((-) (g1 z) y))

i2:: Int -> Int -> Int
i2 x y = if (&&) ((>=) x y) ((>) y 0) then x - y
         else if (&&) ((<) 0 x) ((>) y 0) then 0
         else y - x

-- Ejercicio 3

digitos:: Int -> Int
digitos x = if x < 10 then 1 else digitos (div x 10) + 1

sumaDigitos:: Int -> Int
sumaDigitos x = if x < 10 then x else x `mod` 10 + sumaDigitos (x `div` 10)

reduccion:: Int -> Int
reduccion x = if x < 10 then x else reduccion (sumaDigitos x)

perm:: Int -> Int
perm n 
   | n < 0 = perm (abs n)
   | n == 0 = 1
   | otherwise = perm (n - 1) * n 

var:: Int -> Int -> Int
var n m = perm n `div` perm (n - m)

comb:: Int -> Int -> Int
comb n m = perm n `div` (perm m * perm (n - m))

-- Ejercicio 4

elimina:: Int -> [Int] -> [Int]
elimina n xs = [x | x <- xs, x `mod` n /= 0]

criba:: [Int] -> [Int]
criba [] = []
criba (x:xs) = x : criba (elimina x xs)

primos:: [Int]
primos = criba [2..]

primo:: Int -> Bool
primo n = head (dropWhile (<n) primos) == n

a4:: Int
a4 = head (dropWhile (<=10000) primos)

b4:: Int
b4 = last (take 100 primos)

c4:: [Int]
c4 = take 100 primos