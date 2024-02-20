{-
Princípios de Programação 2022/2023
Trabalho 4

Grupo
 - Rodrigo Correia - 58180
 - Daniel Nunes - 58257
 -}
module Tests
(
    runAllTests
) where

import Labirintos
    ( move,
      GameState(keys, maze, playerPos),
      createGameState,
      getCharInMaze,
      mazeDimensions )
import Test.QuickCheck
    ( Gen, choose, quickCheck, Arbitrary(arbitrary) )
import Control.Monad ( replicateM )
import Data.List ( (\\), nub, sort )
import Utils ( chooseFromList, replaceCharInStringList )
import Data.Char ( isLower, isUpper, toLower )

-- >>-------------------- [ Arbitrary GameState ] --------------------<<
--Gera um labirinto aleatorio
instance Arbitrary GameState where
    arbitrary = do
        mazeLines <- choose (5, 10) :: Gen Int
        mazeColumns <- choose (5, 10) :: Gen Int
        maze <- generateMaze mazeLines mazeColumns
        keys <- generateKeys
        let allPlayerPos = getValidPlayerPos maze
        if null allPlayerPos then
            arbitrary -- no available position for the player -> generate a new GameSate
        else do
            playerPos <- chooseFromList allPlayerPos
            return $ createGameState maze playerPos $ sort (nub keys)

-- Gera uma lista de posiçoes validas para o por player
getValidPlayerPos :: [String] -> [(Int, Int)]
getValidPlayerPos maze = snd $ foldl foldFunc (0, []) maze
    where
        validChars = [' ', '@', 'S', 'F']

        foldFunc :: (Int, [(Int, Int)]) -> String -> (Int, [(Int, Int)])
        foldFunc (i, result) x = (i+1, result ++ zip (repeat i) [y | y <- [0..length x - 1], (x !! y) `elem` validChars])

-- Funcao que gera posicoes especiais (chaves, portas e portais)
generateSpecialPositions :: [(Int, Int)] -> [Char] -> Gen [(Char, (Int, Int))]
generateSpecialPositions allPos cs = generateSpecialPositionsAux allPos cs []
    where
        generateSpecialPositionsAux :: [(Int, Int)] -> [Char] -> [(Char, (Int, Int))] -> Gen [(Char, (Int, Int))]
        generateSpecialPositionsAux [] _ r = return r
        generateSpecialPositionsAux _ [] r = return r
        generateSpecialPositionsAux allPos (c:cs) r = do
            haveChar <- arbitrary :: Gen Bool
            if haveChar || c == 'S' || c == 'F' then
                if c == '@' then
                    if length allPos >= 2 then do
                        pos1 <- chooseFromList allPos
                        pos2 <- chooseFromList (allPos \\ [pos1])
                        generateSpecialPositionsAux (allPos \\ [pos1, pos2]) cs ((c, pos1) : (c, pos2) : r)
                    else
                        generateSpecialPositionsAux allPos cs r
                else do
                    pos <- chooseFromList allPos
                    generateSpecialPositionsAux (allPos \\ [pos]) cs ((c, pos) : r)
            else
                generateSpecialPositionsAux allPos cs r

-- Funcao que gera um labirinto aleatorio
generateMaze :: Int -> Int -> Gen [String]
generateMaze mazeLines mazeColumns = do
    let walls = replicate mazeColumns '*' -- "*****"
    let insidePos = [(x, y) | x <- [1..(mazeLines - 2)], y <- [1..(mazeColumns-2)]] -- []
    specialPos <- generateSpecialPositions insidePos specialChars
    middleLines <- replicateM (mazeLines-2) (generateLine mazeColumns)
    let mazeRaw = [walls] ++ middleLines ++ [walls]
    let fullMaze = getMazeWithSpecials mazeRaw specialPos
    return fullMaze
    where
        specialChars = ['S', 'F', 'A', 'a', 'B', 'b', 'C', 'c', '@']

-- Funcao que coloca os caracteres especiais no labirinto (chaves, portas e portais)
getMazeWithSpecials :: [String] -> [(Char, (Int, Int))] -> [String]
getMazeWithSpecials = foldl (\maze (c, pos) -> replaceCharInStringList maze pos c)

-- Funcao que retorna um tipo de chave
generateKeys :: Gen String
generateKeys = replicateM 3 (choose ('a', 'c') :: Gen Char)

-- Funcao que gera uma linha do labirinto
generateLine :: Int -> Gen String
generateLine size = do
    inside <- replicateM (size-2) $ chooseFromList ['*', ' ']
    return $ "*" ++ inside ++ "*"

-- >>-------------------- [ ValidMoves data ] --------------------<<
-- Gera movimentos aleatorios
data ValidMoves = ValidMoves String

instance Arbitrary ValidMoves where
    arbitrary = do
        amountOfMoves <- choose (1, 100) :: Gen Int
        moves <- replicateM amountOfMoves (choose (1, 4) :: Gen Int)
        return $ ValidMoves $ map (\x -> case x of
                                            1 -> 'r'
                                            2 -> 'l'
                                            3 -> 'd'
                                            _ ->'u') moves

instance Show ValidMoves where
    show (ValidMoves moves) = moves

-- >>-------------------- [ Tests ] --------------------<<
-- Testa se o labirinto nao muda de dimensao apos movimentos
prop_maze_dimensions :: GameState -> ValidMoves -> Bool
prop_maze_dimensions state (ValidMoves []) = True
prop_maze_dimensions state (ValidMoves (c:cs))
    | mazeDimensions state == mazeDimensions movedState = prop_maze_dimensions movedState (ValidMoves cs)
    | otherwise = False
    where
        movedState = move state [c]

-- Testa se o player sai do labirinto apos movimentos
prop_leave_maze :: GameState -> ValidMoves -> Bool
prop_leave_maze state (ValidMoves []) = True
prop_leave_maze state (ValidMoves (c:cs))
    | x > 0 && x < lines && y > 0 && y < cols = prop_maze_dimensions movedState (ValidMoves cs)
    | otherwise = False
    where
        movedState = move state [c]
        (lines, cols) = mazeDimensions state
        (x, y) = playerPos movedState

-- Testa se a lista de chaves funciona corretamente apos movimentos
prop_maze_keys :: GameState -> ValidMoves -> Bool
prop_maze_keys state (ValidMoves []) = True
prop_maze_keys state (ValidMoves (c:cs))
    | length (keys state) <= length (keys movedState) = prop_maze_keys movedState (ValidMoves cs)
    | otherwise = False
    where
        movedState = move state [c]

-- Testa se o player nao passa as portas sem a chave necessaria 
prop_maze_doors :: GameState -> ValidMoves -> Bool
prop_maze_doors state (ValidMoves []) = True
prop_maze_doors state (ValidMoves (c:cs))
    | numDoorsInMaze state >= numDoorsInMaze movedState = prop_maze_keys movedState (ValidMoves cs)
    | otherwise = False
    where
        movedState = move state [c]

        numDoorsInMaze :: GameState -> Int
        numDoorsInMaze state = foldl (\doors x -> doors + numDoorsInString x) 0 $ maze state

        numDoorsInString :: String -> Int
        numDoorsInString [] = 0
        numDoorsInString (x:xs) = if isUpper x then 1 else 0 + numDoorsInString xs

-- Testa se o numero de portais nao altera apos movimentos
prop_move_teleports :: GameState -> ValidMoves -> Bool
prop_move_teleports state (ValidMoves []) = True
prop_move_teleports state (ValidMoves(c:cs))
    | numTeleportsInMaze state == numTeleportsInMaze movedState = prop_move_teleports movedState (ValidMoves cs)
    | otherwise = False
    where
        movedState = move state [c]

        numTeleportsInMaze :: GameState -> Int
        numTeleportsInMaze state = foldl (\doors x -> doors + numTeleportsInString x) 0 $ maze state

        numTeleportsInString :: String -> Int
        numTeleportsInString [] = 0
        numTeleportsInString (x:xs) = if x == '@' then 1 else 0 + numTeleportsInString xs

-- Retorna o movimento a fazer a partir do comando
parseDirection :: Char -> (Int, Int)
parseDirection 'u' = (-1, 0)
parseDirection 'd' = (1, 0)
parseDirection 'r' = (0, 1)
parseDirection _ = (0, -1)


-- passar porta sem chave apos movimentos
prop_move_doors :: GameState -> ValidMoves -> Bool
prop_move_doors _ (ValidMoves []) = True 
prop_move_doors state (ValidMoves (c:cs))
    | isUpper nextChar && nextChar /= 'F' && nextChar /= 'S' && playerPos movedState == nextPos && toLower nextChar `notElem` ['a'..'c'] = False -- && toLower nextChar `notElem` keys state
    | otherwise = prop_move_doors movedState (ValidMoves cs)
    where
        movedState = move state [c]
        (x, y) = playerPos state
        (mx, my) = parseDirection c
        nextPos = (x + mx, y + my)
        nextChar = getCharInMaze (maze state) nextPos

-- pegar chaves e adicionar a lista corretamente apos movimentos
prop_list_keys :: GameState -> ValidMoves -> Bool
prop_list_keys state (ValidMoves []) = True
prop_list_keys state (ValidMoves (c:cs))
    | isLower nextChar && nextChar `notElem` keys movedState = False
    | otherwise = prop_list_keys movedState (ValidMoves cs)
    where
        movedState = move state [c]
        (x, y) = playerPos state
        (mx, my) = parseDirection c
        nextPos = (x + mx, y + my)
        nextChar = getCharInMaze (maze state) nextPos

-- validar movimentos -> nao anda mais de uma casa (a menos q seja portal -> outro portal)
prop_move_path :: GameState -> ValidMoves -> Bool
prop_move_path state (ValidMoves []) = True
prop_move_path state (ValidMoves (c:cs))
    | nextChar == '*' && playerPos movedState == nextPos = False
    | otherwise = prop_list_keys movedState (ValidMoves cs)
    where
        movedState = move state [c]
        (x, y) = playerPos state
        (mx, my) = parseDirection c
        nextPos = (x + mx, y + my)
        nextChar = getCharInMaze (maze state) nextPos

-- testa todos os testes acima
runAllTests :: IO ()
runAllTests = do
    quickCheck prop_maze_dimensions
    quickCheck prop_leave_maze
    quickCheck prop_maze_keys
    quickCheck prop_maze_doors
    quickCheck prop_move_teleports
    quickCheck prop_move_doors
    quickCheck prop_move_path
    quickCheck prop_list_keys