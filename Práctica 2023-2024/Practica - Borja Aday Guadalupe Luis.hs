-- Borja Aday Guadalupe Luis

-- Definiciones de tipos

-- Expresiones

{-
Definimos un tipo expresión aritmetica que o bien puede ser un entero, una variable, una suma, una resta o una multiplicacion
Para ello utilizamos I como la constructora de un entero, V como constructora de variable, y para las operaciones utilizamos
su símbolo correspondiente como operador infijo, asociandoles las prioridades correctas a cada uno. Además las derivamos de show y read
para posteriormente poder leer de un fichero las intrucciones y mostrarlas para ejecutar el programa interactivamente.
-}
data ExpAritmetica = I Integer
                   | V String
                   | ExpAritmetica :+ ExpAritmetica
                   | ExpAritmetica :- ExpAritmetica
                   | ExpAritmetica :* ExpAritmetica
                   deriving (Show, Read)
infixl 6 :+
infixl 6 :-
infixl 7 :*

{-
Definimos un tipo expresión booleana que son todas las posibles comparaciones numéricas, negación, conjunción y disyunción
para ello asignamos a cada operador infijo su prioridad correcta, a excepcion de la negación que al ser unario tenemos una N como constructora.
También derivamos de Show y Read para su posterior ejecución interactiva.
-}
data ExpBooleana = ExpAritmetica :== ExpAritmetica
                 | ExpAritmetica :< ExpAritmetica
                 | ExpAritmetica :<= ExpAritmetica
                 | ExpAritmetica :> ExpAritmetica
                 | ExpAritmetica :>= ExpAritmetica
                 | N ExpBooleana
                 | ExpBooleana :&& ExpBooleana
                 | ExpBooleana :|| ExpBooleana
                 deriving (Show, Read)
infix 4 :==
infix 4 :<
infix 4 :<=
infix 4 :>
infix 4 :>=
infixr 3 :&&
infixr 2 :||


 -- Instrucciones

{-
Definimos un tipo Instrucción que o bien puede ser una asignación, un if o un bucle while. Para ello utilizamos o bien el operador infijo
:= para las asignaciones o la constructora If para los saltos condicionales y While para los bucles. Derivamos de Show y Read por el mismo motivo
que los anteriores.
-}
data Instruccion = String := ExpAritmetica
                 | If ExpBooleana Programa Programa
                 | While ExpBooleana Programa
                 deriving (Show, Read)
infix 1 :=


-- Estado y Programa

{-
Creamos un estado que será una lista de pares (variable, valor) donde asignamos un valor a cada variable, este será el estado real del programa en cada momento
y al finalizar será donde tengamos el valor resultado, en una variable llamada "R". Por otro lado el programa es tan solo una lista de intrucciones gracias a los
tipos creados anteriormente.
-}
type Estado = [(String, Integer)]
type Programa = [Instruccion]



-- Funcion ejecuta

{-ejecuta es una función que recibe un programa y un estado inicial y devuelve un Integer, que será el resultado de buscar la variable R haber ejecutado el programa-}
ejecuta :: Programa -> Estado -> Integer
ejecuta programa estado = buscar "R" (ejecutaPrograma programa estado)

{-
ejecutaPrograma recibe un programa, un estado y devuelve el nuevo estado final tras la ejecución de todas las intrucciones, para ello utilizamos un foldl
donde aplicamos la funcion ejecutaInstruccion a cada instruccion con el estado anterior resultante.
-}
ejecutaPrograma :: Programa -> Estado -> Estado
ejecutaPrograma resto estado = foldl ejecutaInstruccion estado resto

-- Funciones auxiliares

{-
ejecutaInstruccion se encarga de ejecutar la instruccion dada que puede ser asignacion, if o while. 
   * Para la asignacion actualiza en el estado el valor de la variable al resultado de la expresión aritmética dada.
   * Para el if evalua la expresión booleana y dependiendo de su valor llama ejecutaPrograma con el programa correspondiente.
   * Para el while evalua la expresión booleana y mientras esta se cumpla vuelve a ejecutar el programa dentro del while.
-}
ejecutaInstruccion :: Estado -> Instruccion -> Estado
ejecutaInstruccion estado (variable := expr) = actualizar variable (evaluarExpAritmetica expr estado) estado
ejecutaInstruccion estado (If condicion programaTrue programaFalse) =
    if evaluarExpBooleana condicion estado
    then ejecutaPrograma programaTrue estado
    else ejecutaPrograma programaFalse estado
ejecutaInstruccion estado (While condicion programa) =
    if evaluarExpBooleana condicion estado
    then ejecutaInstruccion (ejecutaPrograma programa estado) (While condicion programa)
    else estado


{-
Para evaluar las expresiones aritméticas recibimos la expresión, el estado y devolvemos un entero. En cada caso hacemos lo correspondiente.
Si es un entero directamente devolvemos el entero, si es una variable la buscamos en el estado, para cualquier otra operacion, como pueden ser recursivas,
eveluamos la expresion1 y la expresión2 aplicandole la operación para que el resultado sea el integer.
-}
evaluarExpAritmetica :: ExpAritmetica -> Estado -> Integer
evaluarExpAritmetica (I valor) _ = valor
evaluarExpAritmetica (V variable) estado = buscar variable estado
evaluarExpAritmetica (exp1 :+ exp2) estado = evaluarExpAritmetica exp1 estado + evaluarExpAritmetica exp2 estado
evaluarExpAritmetica (exp1 :- exp2) estado = evaluarExpAritmetica exp1 estado - evaluarExpAritmetica exp2 estado
evaluarExpAritmetica (exp1 :* exp2) estado = evaluarExpAritmetica exp1 estado * evaluarExpAritmetica exp2 estado

{-
Para evaluar las expresiones booleanas recibimos la expresión, el estado y devolvemos un booleano.
En cada caso hacemos lo correspondiente como en la función anterior.
-}
evaluarExpBooleana :: ExpBooleana -> Estado -> Bool
evaluarExpBooleana (exp1 :== exp2) estado = evaluarExpAritmetica exp1 estado == evaluarExpAritmetica exp2 estado
evaluarExpBooleana (exp1 :< exp2) estado = evaluarExpAritmetica exp1 estado < evaluarExpAritmetica exp2 estado
evaluarExpBooleana (exp1 :<= exp2) estado = evaluarExpAritmetica exp1 estado <= evaluarExpAritmetica exp2 estado
evaluarExpBooleana (exp1 :> exp2) estado = evaluarExpAritmetica exp1 estado > evaluarExpAritmetica exp2 estado
evaluarExpBooleana (exp1 :>= exp2) estado = evaluarExpAritmetica exp1 estado >= evaluarExpAritmetica exp2 estado
evaluarExpBooleana (N exp1) estado = not (evaluarExpBooleana exp1 estado)
evaluarExpBooleana (exp1 :&& exp2) estado = evaluarExpBooleana exp1 estado && evaluarExpBooleana exp2 estado
evaluarExpBooleana (exp1 :|| exp2) estado = evaluarExpBooleana exp1 estado || evaluarExpBooleana exp2 estado

{-
La función actualizar recibe un String (nombre de la variable), un integer y un estado y devuelve el nuevo estado. En caso de que el estado sea vacío, 
lo único que hace es crear un nuevo par (variable, valor) con los datos dados. En caso de que el estado no sea vacio, busca hasta encontrar la variable y al encontrarla
cambia su valor, si no la encuentra la añade al final al volver a entrar en la primera ecuación.
-}
actualizar :: String -> Integer -> Estado -> Estado
actualizar variableActualizar nuevoValor [] = [(variableActualizar, nuevoValor)]
actualizar variableActualizar nuevoValor ((v, valor):resto)
    | variableActualizar == v = (variableActualizar, nuevoValor) : resto
    | otherwise = (v, valor) : actualizar variableActualizar nuevoValor resto

{-
La función buscar busca una variable dentro de un estado y devuelve su valor, en caso de no encontrarla da un error.
-}
buscar:: String -> Estado -> Integer
buscar _ [] = error "Variable no encontrada."
buscar variableBuscada ((variable, valor):resto)
   | variableBuscada == variable = valor
   | otherwise = buscar variableBuscada resto



-- Ejemplos

-- Ejemplo factorial del enunciado

factorial = ["Y" := V "X",
             "R" := I 1,
             While (I 0 :< V "Y") ["R" := V "R" :* V "Y", "Y" := V "Y" :- I 1]
            ]

s0 = [("X",3)]

{-
Para comprobar que el resto de instrucciones también funcionan en nuestro programa añadiremos ciertas condiciones
para usar el If, While, negaciones, conjunciones y disyunciones, para ello hacemos un factorial complejo donde solo
lo calculamos si el valor está entre [0,10] o si es igual a 12.
-}
factorialComplejo :: [Instruccion]
factorialComplejo = ["R" := I 1,
                     If (((V "X" :>= I 1) :&& (V "X" :<= I 10)) :|| (V "X" :== I 12))
                        [While (N (V "X" :== I 0)) ["R" := V "R" :* V "X", "X" := V "X" :- I 1]]
                        ["R" := I (-1)]
                    ]

s1 = [("X",0)]
s2 = [("X",3)]
s3 = [("X",12)]

{-
Intrucciones a ejecutar para comprobar el correcto funcionamiento:
ejecuta factorial s0
ejecuta factorialComplejo s1
ejecuta factorialComplejo s2
ejecuta factorialComplejo s3
-}

-- Teniendo la posibilidad de usar el factorial podemos hacer uso para calcular variaciones o combinaciones
variaciones :: Integer -> Integer -> Integer
variaciones m n = ejecuta factorial [("X",m)] `div` ejecuta factorial [("X",m-n)]

combinaciones :: Integer -> Integer -> Integer
combinaciones n m = ejecuta factorial [("X",n)] `div` (ejecuta factorial [("X",m)] * ejecuta factorial [("X",n-m)])



-- Interacciones con el usuario

{-
Los ficheros aceptados son directamente una lista de instrucciones, que representan literalmente un Programa.

Ejemplo de formato de fichero:

["Y" := V "X",
 "R" := I 1,
 While (I 0 :< V "Y") 
       ["R" := V "R" :* V "Y", 
        "Y" := V "Y" :- I 1
       ]
]
-}

{-
ejecutarProgramaInteractivo recibe un programa y un estado y devuelve un Estado de E/S, si el programa ha terminado (lista de instrucciones vacía) devuelve el estado sin más,
En otro caso, imprime el estado actual, te muestra la instrucción actual a ejecutar, la ejecuta y muestra el estado resultante de ejecutar esa instrucción con ese estado.
Para ejecutar la siguiente instrucción espera a que el usuario presione el enter.
-}
ejecutaProgramaInteractivo :: Programa -> Estado -> IO Estado
ejecutaProgramaInteractivo [] estado = return estado
ejecutaProgramaInteractivo (instruccion:resto) estado = do
    putStrLn $ "Estado actual: " ++ show estado
    putStrLn $ "Ejecutando instrucción: " ++ show instruccion
    let nuevoEstado = ejecutaInstruccion estado instruccion
    if null resto
    then putStrLn $ "Estado final: " ++ show nuevoEstado
    else putStrLn $ "Nuevo estado: " ++ show nuevoEstado
    _ <- getLine  -- Espera a que el usuario presione Enter
    ejecutaProgramaInteractivo resto nuevoEstado

{-
En el main solicitamos al usuario un nombre de fichero donde se encuentra el programa escrito como una lista de instrucciones, de modo que podemos usar la instrucción read y convertirlo
directamente en un Programa, también se le solicita el estado inicial de idéntica forma como una lista de pares clave valor [("variable", valor)] para poder usar read.
Una vez el usuario nos ha dado esos datos, ejecutamos el programa interactivamente para ir viendo paso a paso lo que ocurre.
-}
main :: IO ()
main = do
    -- Preguntar por el nombre del fichero
    putStr "Introduce el nombre del fichero con el programa: "
    nombreFichero <- getLine
    contenidoFichero <- readFile nombreFichero
    let programa = read contenidoFichero :: Programa

    -- Obtener el estado inicial
    putStr "Introduce el estado inicial: "
    estadoInicialStr <- getLine
    putStrLn ""
    let estadoInicial = read estadoInicialStr :: Estado

    -- Ejecutar el programa
    _ <- ejecutaProgramaInteractivo programa estadoInicial
    return ()