-- Borja Aday Guadalupe Luis

-- Ejercicio 1
a1 :: [a] -> a
a1 = foldl1 (\_ x -> x)

b1 :: [a] -> [a]
b1 = foldl (\x y -> y : x) []

c1 :: (a -> Bool) -> [a] -> Bool
c1 f xs = foldl (\x y -> if f y then x else False) True xs

d1 :: Ord a => [a] -> a
d1 xs = foldl (\x y -> if y < x then y else x) (head xs) xs

e1 :: (a -> b) -> [a] -> [b]
e1 f xs = foldr (\x y -> f x : y) [] xs

f1 :: (a -> Bool) -> [a] -> [a]
f1 f = foldr (\x y -> if f x then x : y else y) []

g1 :: (a -> Bool) -> [a] -> [a]
g1 f xs = foldr (\x y -> if f x then x : y else []) [] xs

-- Ejercicio 2

a2 :: (a -> a -> a) -> [a] -> a
a2 f (x:xs) = foldl f x xs

b2 :: (a -> a -> a) -> [a] -> a
b2 f l = foldr f (last l) (init l)

-- Ejercicio 3
a3 :: [Integer]
a3 = [y | x <- [1..], y <- [x, -x]]

b3 :: [(Integer, Integer)]
b3 = zip [x | y <- [0..], x <- [0..y]]  [x | y <- [0..], x <- [y,y-1..0]]

-- Ejercicio 4

a4 :: [a] -> [[a]]
a4 = foldr f []
   where f elem [] = [[elem], []]
         f elem (x:xs) = (elem:x) : (x:xs)
