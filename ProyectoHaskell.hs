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
type Cinta = [String] --tambien se puede usar seq . para que sea mas facil escribirla

type Alfabeto = [String]

ejecutarPaso :: Maquina -> Alfabeto -> Cinta -> String -> Int -> (Cinta, String, Int)
ejecutarPaso maquina alfabeto cinta estadoInicial posicion = procesarEstado (H.lookup estadoInicial maquina) cinta posicion

procesarEstado :: Maybe TMEstado -> Cinta -> Int -> (Cinta, String, Int)
procesarEstado (Just (Estado symTable)) cinta posicion = aumenarCinta $ procesarInstruccion (H.lookup (cinta!!posicion) symTable) cinta posicion
procesarEstado Nothing cinta posicion = ([""],"FIN",-9)

procesarInstruccion :: Maybe TMInstruccion -> Cinta -> Int -> (Cinta, String,Int)
procesarInstruccion (Just (Instruccion escribirSimbolo dir proximoEstado)) cinta posicion
    | dir == CintaDerecha = (remplazarEnIndice posicion escribirSimbolo cinta, proximoEstado, (posicion-1))
    | otherwise = (remplazarEnIndice posicion escribirSimbolo cinta, proximoEstado, (posicion+1))

--Lo unico que hace es verificar si esta en el final o en el principio de la lista, en caso de que asi sea, agrega un elemento al final o al principio.
aumenarCinta (cinta, proximoEstado, posicion)
    | posicion < 0 = ([""] ++ cinta, proximoEstado, posicion+1)
    | posicion == (length cinta) = (cinta ++ [""], proximoEstado, posicion)
    | otherwise = (cinta, proximoEstado, posicion)

--Funcion principal, y llama a la funcion que hace todo el procesamiento de la maquina (bucle)
ejecutar :: Maquina -> Alfabeto -> Cinta -> String -> Int -> IO ()
ejecutar maquina alfabeto cinta estadoInicial maximaCantidadPasos = bucle 0 maximaCantidadPasos maquina alfabeto cinta estadoInicial 0

--bucle recibe la maxima cantidad de pasos, la maquina, alfabeto, cinta, estado inicial y posicion y hace todo el procesamiento de la maquina
--declara una tupla que va dandole valores llamando la funcion ejecutar paso y verifica si el nuevo estado es fin y la cantidad de pasos es menor a la cantidad
--que se informo por teclado, informa que no es interesante, si la cantidad de pasos es igual a la informada, dice que es interesante, y si no se da ninguna de 
--las dos condiciones, vuelve a llamarse recursivamente hasta que alguna de las dos condiciones se cumpla
bucle :: Int -> Int ->  Maquina -> Alfabeto -> Cinta -> String -> Int -> IO ()
bucle i maximaCantidadPasos maquina alfabeto cinta estadoInicial posicion =
                           do 
                               let (nuevaCinta, nuevoEstado, newPosition) = ejecutarPaso maquina alfabeto cinta estadoInicial posicion in
                                 if nuevoEstado == "FIN" && newPosition == -9
                                        then putStr ("El estado ingresado no existe en la Maquina de Turing. \n")
                                 else
                                      if nuevoEstado == "FIN" && i < maximaCantidadPasos - 1
                                            then putStr ("Cantidad de pasos: " ++ show (i+1) ++ ". Programa Completo. La maquina no es interesante " ++ "\n")
                                      else 
                                            if i == maximaCantidadPasos - 1 
                                                then putStr ("Cantidad de pasos: " ++ show (i+1) ++ ".Programa Completo. La maquina es interesante " ++ "\n")
                                            else bucle (i+1) maximaCantidadPasos maquina alfabeto nuevaCinta nuevoEstado newPosition

-- From: http://stackoverflow.com/questions/10133361/haskell-replace-element-in-list
remplazarEnIndice :: Int -> String -> Cinta -> Cinta
remplazarEnIndice n item ls = a ++ (item:b) where (a, (_:b)) = splitAt n ls

alfabeto = ["", "0", "1"]
cinta = ["1", "1", "1","0","1","0","1"]
maquina = H.fromList [ 
  ("0",Estado (H.fromList [
    ("",Instruccion "" CintaDerecha "1"),
    ("0",Instruccion "0" CintaIzquierda "0"), 
    ("1",Instruccion "1" CintaIzquierda "0")])),
  ("1",Estado (H.fromList [
    ("",Instruccion "1" CintaIzquierda "2"), 
    ("0",Instruccion "1" CintaDerecha "2"), 
    ("1",Instruccion "0" CintaDerecha "1")])),
  ("2",Estado (H.fromList [
    ("",Instruccion "" CintaDerecha "FIN"),
    ("0",Instruccion "0" CintaIzquierda "2"), 
    ("1",Instruccion "1" CintaIzquierda "2")]))
  ]

maquina1 = H.fromList [ 
  ("0",Estado (H.fromList [
    ("",Instruccion "" CintaDerecha "1"),
    ("0",Instruccion "1" CintaIzquierda "1"), 
    ("1",Instruccion "0" CintaIzquierda "0")])),
  ("1",Estado (H.fromList [
    ("",Instruccion "0" CintaIzquierda "0"), 
    ("0",Instruccion "1" CintaDerecha "2"), 
    ("1",Instruccion "1" CintaDerecha "1")])),
  ("2",Estado (H.fromList [
    ("",Instruccion "1" CintaDerecha "2"),
    ("0",Instruccion "" CintaIzquierda "2"), 
    ("1",Instruccion "1" CintaIzquierda "FIN")]))
  ]

maquina2 = H.fromList [ 
  ("0",Estado (H.fromList [
    ("",Instruccion "" CintaDerecha "1"),
    ("0",Instruccion "1" CintaIzquierda "1"), 
    ("1",Instruccion "0" CintaIzquierda "0")])),
  ("1",Estado (H.fromList [
    ("",Instruccion "0" CintaIzquierda "0"), 
    ("0",Instruccion "1" CintaDerecha "2"), 
    ("1",Instruccion "1" CintaDerecha "1")])),
  ("2",Estado (H.fromList [
    ("",Instruccion "1" CintaDerecha "1"),
    ("0",Instruccion "" CintaIzquierda "1"), 
    ("1",Instruccion "1" CintaIzquierda "FIN")]))
  ]
