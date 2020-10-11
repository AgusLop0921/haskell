import qualified Data.HashMap.Strict as H

type TMSymTable = (H.HashMap String TMInstruction)

data TMState = Estado TMSymTable
  deriving (Show, Eq)

data TMInstruction = Instruccion String Direction String
  deriving (Show, Eq)

data Direction = CintaDerecha
               | CintaIzquierda
  deriving (Show, Eq)

type Maquina = (H.HashMap String TMState)
type Cinta = [String]
type Alfabeto = [String]

ejecutarPaso :: Maquina -> Alfabeto -> Cinta -> String -> Int -> (Cinta, String, Int)
ejecutarPaso maquina alfabeto cinta estadoInicial position = procesarEstado (H.lookup estadoInicial maquina) cinta position


procesarEstado (Just (Estado symTable)) cinta position = handleAddingTape (procesarInstruccion (H.lookup (cinta!!position) symTable) cinta position)

procesarInstruccion (Just (Instruccion writeSymbol dir nextState)) cinta position
    | dir == CintaDerecha = (remplazarEnIndice position writeSymbol cinta, nextState, (position-1))
    | otherwise = (remplazarEnIndice position writeSymbol cinta, nextState, (position+1))

handleAddingTape (cinta, nextState, position)
    | position < 0 = ("":cinta, nextState, position+1)
    | position == (length cinta) = (cinta ++ [""], nextState, position)
    | otherwise = (cinta, nextState, position)

runVerbose :: Maquina -> Alfabeto -> Cinta -> String -> IO ()
runVerbose maquina alfabeto cinta estadoInicial = loop 0 maquina alfabeto cinta estadoInicial 0

loop :: Int -> Maquina -> Alfabeto -> Cinta -> String -> Int -> IO ()
loop i maquina alfabeto cinta estadoInicial position =
                           do  putStr ("Estado " ++ show i ++ ". Cinta: [" ++ showTape cinta position "" 0 ++ "]" ++ "\n" ++ "Estado " ++ show i ++ ". En estado " ++ show estadoInicial ++ "\n"
                                        -- ++ ": [" ++ showState alfabeto (H.lookup estadoInicial maquina) ++ "]\n"
                                        ++ "Estado " ++ show i ++ ". " ++ showInstruction maquina alfabeto cinta estadoInicial position ++ "\n\n")
                               let (newTape, newState, newPosition) = ejecutarPaso maquina alfabeto cinta estadoInicial position in
                                   if newState == "HALT"
                                        then putStr ("Estado " ++ show (i+1) ++ ". Cinta: [" ++ showTape newTape newPosition "" 0 ++ "]" ++ "\n" ++ "Estado " ++ show (i+1) ++ ". DETENER Programa Completo." ++ "\n")
                                   else loop (i+1) maquina alfabeto newTape newState newPosition



showTape :: Cinta -> Int -> String -> Int -> String
showTape [] position str i = str
showTape (a:cinta) position str i
    | position == i = showTape cinta position (str ++ ((getPrefix i) ++ "*" ++ show a)) (i+1)
    | otherwise = showTape cinta position (str ++ ((getPrefix i) ++ show a)) (i+1)

getPrefix :: Int -> String
getPrefix i
    | i > 0 = ", "
    | otherwise = ""

addToPrintStr :: Maybe TMInstruction -> String -> String -> String
addToPrintStr (Just (Instruccion symbol dir nextState)) readSym str = (str ++ "(read " ++ show readSym ++ ", write " ++ show symbol ++ ", movimiento " ++ show dir ++ ", proximo estado: " ++ show nextState ++ ") ")
addToPrintStr Nothing readSym str = str

showInstruction maquina alfabeto cinta estadoInicial position = showInstructionHelper (H.lookup estadoInicial maquina) cinta position

showInstructionHelper (Just (Estado symTable)) cinta position = (showInstructionHelper2 (H.lookup (cinta!!position) symTable) (cinta!!position))

showInstructionHelper2 (Just (Instruccion writeSymbol dir nextState)) readSymbol = ("Instruccion: ( Leyendo  " ++ show readSymbol ++ ", write " ++ show writeSymbol ++ ", movimiento " ++ show dir ++ ", proximo estado: " ++ show nextState ++ ")")


-- From: http://stackoverflow.com/questions/10133361/haskell-replace-element-in-list
remplazarEnIndice :: Int -> String -> [String] -> [String]
remplazarEnIndice n item ls = a ++ (item:b) where (a, (_:b)) = splitAt n ls


alfabeto = ["", "0", "1"]
cinta = ["1", "1", "1"]
maquina = H.fromList [
                        ("0",Estado (H.fromList [("",Instruccion "" CintaDerecha "1"), ("0",Instruccion "0" CintaIzquierda "0"), ("1",Instruccion "1" CintaIzquierda "0")])),("1",Estado (H.fromList [("",Instruccion "1" CintaIzquierda "2"), ("0",Instruccion "1" CintaDerecha "2"), ("1",Instruccion "0" CintaDerecha "1")])),("2",Estado (H.fromList [("",Instruccion "" CintaDerecha "HALT"), ("0",Instruccion "0" CintaIzquierda "2"), ("1",Instruccion "1" CintaIzquierda "2")]))]




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

