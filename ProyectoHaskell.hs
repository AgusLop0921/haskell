import qualified Data.HashMap.Strict as H

type TMSymTable = (H.HashMap String TMInstruccion)

data TMEstado = Estado TMSymTable
  deriving (Show, Eq)

data TMInstruccion = Instruccion String Direccion String
  deriving (Show, Eq)

data Direccion = CintaDerecha
               | CintaIzquierda
  deriving (Show, Eq)

type Maquina = (H.HashMap String TMEstado)
type Cinta = [String]
type Alfabeto = [String]

ejecutarPaso :: Maquina -> Alfabeto -> Cinta -> String -> Int -> (Cinta, String, Int)
ejecutarPaso maquina alfabeto cinta estadoInicial posicion = procesarEstado (H.lookup estadoInicial maquina) cinta posicion

procesarEstado :: Maybe TMEstado -> Cinta -> Int -> (Cinta, String, Int)
procesarEstado (Just (Estado symTable)) cinta posicion = adicionACinta (procesarInstruccion (H.lookup (cinta!!posicion) symTable) cinta posicion)

procesarInstruccion :: Maybe TMInstruccion -> Cinta -> Int -> ([String], String,Int)
procesarInstruccion (Just (Instruccion escribirSimbolo dir proximoEstado)) cinta posicion
    | dir == CintaDerecha = (remplazarEnIndice posicion escribirSimbolo cinta, proximoEstado, (posicion-1))
    | otherwise = (remplazarEnIndice posicion escribirSimbolo cinta, proximoEstado, (posicion+1))

adicionACinta (cinta, proximoEstado, posicion)
    | posicion < 0 = ("":cinta, proximoEstado, posicion+1)
    | posicion == (length cinta) = (cinta ++ [""], proximoEstado, posicion)
    | otherwise = (cinta, proximoEstado, posicion)

ejecutar :: Maquina -> Alfabeto -> Cinta -> String -> IO ()
ejecutar maquina alfabeto cinta estadoInicial = bucle 0 maquina alfabeto cinta estadoInicial 0

bucle :: Int -> Maquina -> Alfabeto -> Cinta -> String -> Int -> IO ()
bucle i maquina alfabeto cinta estadoInicial posicion =
                           do  putStr ("Estado " ++ show i ++ ". Cinta: [" ++ mostrarCinta cinta posicion "" 0 ++ "]" ++ "\n" ++ "Estado " ++ show i ++ ". En estado " ++ show estadoInicial ++ "\n"
                                        -- ++ ": [" ++ showState alfabeto (H.lookup estadoInicial maquina) ++ "]\n"
                                        ++ "Estado " ++ show i ++ ". " ++ mostrarInstruccion maquina alfabeto cinta estadoInicial posicion ++ "\n\n")
                               let (nuevaCinta, nuevoEstado, newPosition) = ejecutarPaso maquina alfabeto cinta estadoInicial posicion in
                                   if nuevoEstado == "HALT"
                                        then putStr ("Estado " ++ show (i+1) ++ ". Cinta: [" ++ mostrarCinta nuevaCinta newPosition "" 0 ++ "]" ++ "\n" ++ "Estado " ++ show (i+1) ++ ". DETENER Programa Completo." ++ "\n")
                                   else bucle (i+1) maquina alfabeto nuevaCinta nuevoEstado newPosition

mostrarCinta :: Cinta -> Int -> String -> Int -> String
mostrarCinta [] posicion str i = str
mostrarCinta (a:cinta) posicion str i
    | posicion == i = mostrarCinta cinta posicion (str ++ ((obtenerPrefijo i) ++ "*" ++ show a)) (i+1)
    | otherwise = mostrarCinta cinta posicion (str ++ ((obtenerPrefijo i) ++ show a)) (i+1)

obtenerPrefijo :: Int -> String
obtenerPrefijo i
    | i > 0 = ", "
    | otherwise = ""

agregarCadenaImpresion :: Maybe TMInstruccion -> String -> String -> String
agregarCadenaImpresion (Just (Instruccion simbolo dir proximoEstado)) readSym str = (str ++ "(read " ++ show readSym ++ ", write " ++ show simbolo ++ ", movimiento " ++ show dir ++ ", proximo estado: " ++ show proximoEstado ++ ") ")
agregarCadenaImpresion Nothing readSym str = str

mostrarInstruccion maquina alfabeto cinta estadoInicial posicion = mostrarInstruccionAux1 (H.lookup estadoInicial maquina) cinta posicion

mostrarInstruccionAux1 (Just (Estado symTable)) cinta posicion = (mostrarInstruccionAux2 (H.lookup (cinta!!posicion) symTable) (cinta!!posicion))

mostrarInstruccionAux2 (Just (Instruccion escribirSimbolo dir proximoEstado)) readSymbol = ("Instruccion: ( Leyendo  " ++ show readSymbol ++ ", write " ++ show escribirSimbolo ++ ", movimiento " ++ show dir ++ ", proximo estado: " ++ show proximoEstado ++ ")")

-- From: http://stackoverflow.com/questions/10133361/haskell-replace-element-in-list
remplazarEnIndice :: Int -> String -> [String] -> [String]
remplazarEnIndice n item ls = a ++ (item:b) where (a, (_:b)) = splitAt n ls

alfabeto = ["", "0", "1"]
cinta = ["1", "1", "1"]
maquina = H.fromList [ ("0",Estado (H.fromList [("",Instruccion "" CintaDerecha "1"), ("0",Instruccion "0" CintaIzquierda "0"), ("1",Instruccion "1" CintaIzquierda "0")])),("1",Estado (H.fromList [("",Instruccion "1" CintaIzquierda "2"), ("0",Instruccion "1" CintaDerecha "2"), ("1",Instruccion "0" CintaDerecha "1")])),("2",Estado (H.fromList [("",Instruccion "" CintaDerecha "HALT"), ("0",Instruccion "0" CintaIzquierda "2"), ("1",Instruccion "1" CintaIzquierda "2")]))]

--import System.Random
--import System.Random (randomRIO)


--generateListOfTuples [] = []
--generateListOfTuples ((a,b,c):xs) = (a,b,c):xs

 
--pick :: [a] -> IO a
--pick xs = fmap (xs !!) $ randomRIO (0, length xs - 1)

--generateRandomValue :: IO Int
--generateRandomValue = pick [0,1]

--generateRandomMovement :: IO [Char]
--generateRandomMovement = pick ["<",">"]

--generateRandomState :: IO [Char]
--generateRandomState = pick ["a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z"]