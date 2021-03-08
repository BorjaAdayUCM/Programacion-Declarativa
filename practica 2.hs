--Borja Aday Guadalupe Luis

--Ejercicio 1
a1 :: (Num a, Eq a) => a -> [a]
a1 n = a1_aux n []

a1_aux :: (Num a, Eq a) => a -> [a] -> [a]
a1_aux n xs
   | n == 0 = 0:xs
   | otherwise = a1_aux (n-1) ((n^2):xs)

b1 :: (Num a, Eq a) => a -> [(a, a)]
b1 n = reverse (b1_aux n [])

b1_aux :: (Num a, Eq a) => a -> [(a, a)] -> [(a, a)]
b1_aux n xs
   | n == 0 = (0,0):xs
   | otherwise = b1_aux (n-1) ((n,n^2):xs)

c1 :: (Eq a, Floating a) => a -> a
c1 n
   | n == 1 = abs (cos n)
   | otherwise = c1 (n-1) + (n * abs (cos n))

--d1 n
--   | n < 3 = 0
--   | (mod n 3 == 0 || n mod 5 == 0) = (d1 (n-1)) + n
--   | otherwise = (d1 (n-1))

--Ejercicio 2
a2 :: (Num a, Enum a) => a -> [a]
a2 n = [(x^2) | x <- [0..n]]

b2 :: (Num b, Enum b) => b -> [(b, b)]
b2 n = [(x, x^2) | x <- [n, n-1..0]]

c2 :: (Enum a, Floating a) => a -> a
c2 n = sum [x * abs (cos (x)) | x <- [1..n]]

d2 :: Integral a => a -> a
d2 n = sum [x | x <- [1..n], x `mod` 3 == 0 || x `mod` 5 == 0]

e2 :: Integral a => a -> [a]
e2 n = [(x^3) | x <- [1..n], (x^3) `mod` 100 == 43 && (x^3) < n]