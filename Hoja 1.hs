Ejercicio 1

[True : []]
[[True]]
[[Boolean]]

[] : [True]
Error

[True] : []
[[True]]
[[Boolean]]

True : [ True ]
[True, True]
[Boolean]

1 : (2 : 3 : [ ])
1 : (2 : [3])
1 : [2,3]
[1,2,3]
Num a => [a]

[1 : [2]] : [ [ ] ]
[[1,2]] : [[]]
[[[1,2]], []]
Num a => [[[a]]]

[1, 1] : (2 : [ ])
[1,1] : [2]
Error

[ ] : [ [ ] ] : [ ]
[] : [[[]]]
[[], [[]]]
[[[a]]]

Ejercicio 2
head ['a', 'f'] :: Char
tail ['a', 'f'] :: Char
tail head "af" :: Error
head (tail "af") :: Char
splitAt 4 ['a' .. 'f'] :: ([Char], [Char])
zip [3 + 2, 0] ["af"] :: Num a => [(a, [Char])]
drop (+2) [1,2,3] :: Error falta el otro operando del +
drop (div 2 0) [1,2,3] :: Error div 2 0
i1) 'ab' ++ 'bc' :: Error
i2) "ab" ++ "bc" :: [Char]
i3) "ab" + "bc" :: Error
i4) "ab" ++ 'c' :: Error

Ejercicio 3
head ['a', 'f'] :: 'a'
tail ['a', 'f'] :: 'f'
tail head "af" :: Error
head (tail "af") :: 'f'
splitAt 4 ['a' .. 'f'] :: ("abcd", "ef")
zip [3 + 2, 0] ["af"] :: [(5, "af")]
drop (+2) [1,2,3] :: Error falta el otro operando del +
drop (div 2 0) [1,2,3] :: Error div 2 0
i1) 'ab' ++ 'bc' :: Error
i2) "ab" ++ "bc" :: "abbc"
i3) "ab" + "bc" :: Error
i4) "ab" ++ 'c' :: Error

Ejercicio 4
1 - d
0:2:[4] - 0:(2:(4:[]))

a
[[[0],[2,4]]]

2 - c
[0]:([2:(4:[])]) 
[0]:([2:[4]])
[0]:[[2,4]]
[[0], [2,4]]

b - 4
[0]:(2:[4]):[[]]
[0]:[2, 4]:[[]]
[0]:[[2, 4], []]
[[0], [2, 4], []]

3 - a
[[0] : (2:[4]) : []]
[[0] : [2,4] : []]
[[0] : [[2,4]]]
[[[0], [2,4]]]]

c - 2
[0]:([2:(4:[])])
[0]:([2:[4]])
[0]:[[2,4]]
[[0], [2,4]]

4 - b
[0]:(2:4:[]):[[]]
[0]:[2,4]:[[]]
[0]:[[2,4],[]]
[[0],[2,4],[]]

Ejercicio 5
let x = y + 1 in let z = x ^ 2 in z --> Y no existe
let y = let x = 2 in (let z = x ^ 2 in z) in y --> 4
let y = let x = 2 in (let z = x ^ 2 in z) in z + y --> Z no existe fuera del let
let {x = 5; y = 4} in if x < y then x else y --> 4
let {x = 5; y = 4} in if x < y then z = x else z = y --> Error debe devolver valor, no asignaciones
if [1] !! 1 == 1 then [1] else [ ] --> Error, IndexOutOfBoundsException
let x = elem 1 [1] in if x then [1] else [ ] --> [1]
let x = elem 1 [ ] in if x then [1] !! 1 else [1] !! 0 --> 1
let x = elem 1 [ ] in if x then 1 else [ ] --> 

Ejercicio 7
f :: Ord a => a -> a -> a -> (a, a, a)
f x y z = if x < y then if x < z then if y < z then (x,y,z) else (x,z,y) else (z,x,y) else if x < z then (y,x,z) else if z < y then (z,y,x) else (y,z,x)

f :: Ord a => a -> a -> a -> (a, a, a)
f x y z
    | x < y && y < z = (x,y,z)
    | x < z && z < y = (x,z,y)
    | y < x && x < z = (y,x,z)
    | y < z && z < x = (y,z,x)
    | z < x && x < y = (z,x,y)
    | z < y && y < x = (z,y,x)

ordenaGuar :: Integral a => a -> a -> a -> (a, a, a)
ordenaGuar x y z
    | (x <= y && y <= z) = (x, y, z)
    | x < y = ordenaGuar x z y
    | otherwise = ordenaGuar z x y


Ejercicio 8

f :: Ord a => (a, a, a) -> a
f (x, y, z) = let m = min (min x y) z in m

f :: Ord a => a -> a -> a -> a
f x y z = let m = min (min x y) z in m

Ejercicio 9
f x y z = if x < y then if x < z then if y < z then (x,y,z) else (x,z,y) else (z,x,y) else if x < z then (y,x,z) else if z < y then (z,y,x) else (y,z,x)