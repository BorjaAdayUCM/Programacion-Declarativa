-- Borja Aday Guadalupe Luis

-- Ejercicio 1

data Direccion = ARRIBA | ABAJO | IZQUIERDA | DERECHA deriving (Eq, Ord, Show)

mueve :: (Integer, Integer) -> Direccion -> (Integer, Integer)
mueve (x,y) ARRIBA = if x == 0 then (x, y) else (x - 1, y)
mueve (x,y) ABAJO = if x == 100 then (x, y) else (x + 1, y)
mueve (x,y) IZQUIERDA = if y == 0 then (x, y) else (x, y - 1)
mueve (x,y) DERECHA = if y == 100 then (x, y) else (x, y + 1)

destino :: (Integer, Integer) -> [Direccion] -> (Integer, Integer)
destino p xs = foldl (mueve) p xs

trayectoria :: (Integer, Integer) -> [Direccion] -> [(Integer, Integer)]
trayectoria p xs = tail (foldl (\list x -> list ++ [mueve (last list) x]) [p] xs)

-- Ejercicio 2

data Nat = Zero | Suc Nat deriving (Eq, Ord)

(+++) :: Nat -> Nat -> Nat
Zero +++ x = x
(Suc x) +++ y = x +++ (Suc y)

(***) :: Nat -> Nat -> Nat
Zero *** x = Zero
(Suc Zero) *** x = x
(Suc x) *** y = y +++ (x *** y)

natToInt :: Nat -> Integer
natToInt Zero = 0
natToInt (Suc x) = 1 + (natToInt x)

instance Show Nat where
    show x = show (natToInt x)

-- Ejercicio 3

data NumComplejo = Complejo (Int, Int) deriving (Eq)

instance Show NumComplejo where
    show (Complejo (x, y)) = if y >= 0 then (show x) ++ "+" ++ (show y) ++ "i" else (show x) ++ (show y) ++ "i"

instance Num NumComplejo where
    Complejo (x, y) + Complejo (z, w) = Complejo (x + z, y + w)
    Complejo (x, y) - Complejo (z, w) = Complejo (x - z, y - w)
    Complejo (x, y) * Complejo (z, w) = Complejo (x * z - y * w, x * y + y * z)

--instance Fractional NumComplejo where
    --Complejo (x, y) / Complejo (z, w) = Complejo ((fromIntegral (x * z + y * w)) / (fromIntegral(z^2 + w^2)), fromIntegral((-x * w + z * y)) / (fromIntegral(z^2 + w^2)))

-- Ejercicio 4

class Medible a where
    medida :: a -> Int

instance Medible Bool where
    medida False = 0
    medida True = 1

instance Medible a => Medible [a] where
    medida [] = 0
    medida (x:xs) = (medida x) + (medida xs)