import System.Random
import System.Random (randomRIO)


generateListOfTuples [] = []
generateListOfTuples ((a,b,c):xs) = (a,b,c):xs

 
pick :: [a] -> IO a
pick xs = fmap (xs !!) $ randomRIO (0, length xs - 1)

generateRandomValue :: IO Int
generateRandomValue = pick [0,1]

generateRandomMovement :: IO [Char]
generateRandomMovement = pick ["<",">"]

generateRandomState :: IO [Char]
generateRandomState = pick ["a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z"]

