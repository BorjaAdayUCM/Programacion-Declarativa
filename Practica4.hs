-- Borja Aday Guadalupe Luis

-- Ejercicio 1

data Direccion = ARRIBA | ABAJO | IZQUIERDA | DERECHA deriving (Eq, Ord, Show)

destino :: (Integer, Integer) -> Direccion -> (Integer, Integer)
destino (x,y) ARRIBA = (x, y + 1)
destino (x,y) ABAJO = (x, y - 1)
destino (x,y) IZQUIERDA = (x - 1, y)
destino (x,y) DERECHA = (x + 1, y)

destinoLista :: (Integer, Integer) -> [Direccion] -> (Integer, Integer)
destinoLista p [] = p
destinoLista p (x:xs) = destinoLista (destino p x) xs

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

-- Ejercicio 4

class Medible a where
    medida :: a -> Int

instance Medible Bool where
    medida False = 0
    medida True = 1

instance Medible a => Medible [a] where
    medida [] = 0
    medida (x:xs) = (medida x) + (medida xs)