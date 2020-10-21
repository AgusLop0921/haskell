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

ejecutar :: Maquina -> Alfabeto -> Cinta -> String -> Int -> IO ()
ejecutar maquina alfabeto cinta estadoInicial paso = bucle 0 paso maquina alfabeto cinta estadoInicial 0

bucle :: Int -> Int ->  Maquina -> Alfabeto -> Cinta -> String -> Int -> IO ()
bucle i paso maquina alfabeto cinta estadoInicial posicion =
                           do 
                               let (nuevaCinta, nuevoEstado, newPosition) = ejecutarPaso maquina alfabeto cinta estadoInicial posicion in
                                   if nuevoEstado == "FIN" && i < paso
                                        then putStr ("Paso " ++ show (i+1) ++ ".Programa Completo. La maquina no es interesante " ++ "\n")
                                   else 
                                        if i == paso 
                                            then putStr ("Paso " ++ show (i+1) ++ ".Programa Completo. La maquina es interesante " ++ "\n")
                                        else bucle (i+1) paso maquina alfabeto nuevaCinta nuevoEstado newPosition


-- From: http://stackoverflow.com/questions/10133361/haskell-replace-element-in-list
remplazarEnIndice :: Int -> String -> [String] -> [String]
remplazarEnIndice n item ls = a ++ (item:b) where (a, (_:b)) = splitAt n ls

alfabeto = ["", "0", "1"]
cinta = ["1", "1", "1"]
maquina = H.fromList [ ("0",Estado (H.fromList [("",Instruccion "" CintaDerecha "1"), ("0",Instruccion "0" CintaIzquierda "0"), ("1",Instruccion "1" CintaIzquierda "0")])),("1",Estado (H.fromList [("",Instruccion "1" CintaIzquierda "2"), ("0",Instruccion "1" CintaDerecha "2"), ("1",Instruccion "0" CintaDerecha "1")])),("2",Estado (H.fromList [("",Instruccion "" CintaDerecha "FIN"), ("0",Instruccion "0" CintaIzquierda "2"), ("1",Instruccion "1" CintaIzquierda "2")]))]
