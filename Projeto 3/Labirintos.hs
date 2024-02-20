{-
Princípios de Programação 2022/2023
Trabalho 4

Grupo
 - Rodrigo Correia - 58180
 - Daniel Nunes - 58257
 -}
module Labirintos
(
GameState (..), -- funções maze, playerPos e keys
createGameState,
move,
mazeDimensions,
getCharInMaze
) where

import Data.Char ( isLower, toLower )
import Data.List ( elemIndex, nub, sort )
import Utils ( replaceCharInStringList )

data GameState = GameState {
    maze :: [String],
    playerPos :: (Int, Int),
    keys :: String
}

-- >>------------------------- [ Show GameState ] -------------------------<<
instance Show GameState where
    show state = unlines (getMazeWithPlayer state) ++ "chaves: " ++ keys state

-- Devolve o labirinto com o caracter do player na posição dele 
getMazeWithPlayer :: GameState -> [String]
getMazeWithPlayer (GameState maze pos _) = replaceCharInStringList maze pos 'P'


-- >>------------------------- [ Read GameState ] -------------------------<<
instance Read GameState where
    readsPrec _ str = [(readFromStringList (lines str), "")]

-- Dá read de um GameState a partir de uma lista de Strings (esta será a lida no ficheiro)
readFromStringList :: [String] -> GameState
readFromStringList (pos:keys:xs) = createGameState xs (read pos) keys
readFromStringList _ = error "Invalid string format!"


-- >>------------------------- [ Exported Functions ] -------------------------<<

-- Cria um novo GameState com os dados passados
createGameState :: [String] -> (Int, Int) -> String -> GameState
createGameState = GameState

-- Move o jogador num conjunto de direções
move :: GameState -> String -> GameState
move state [] = state
move state (x:xs)
    | x == 'u' = move (applyMove state (-1, 0)) xs
    | x == 'd' = move (applyMove state (1, 0)) xs
    | x == 'l' = move (applyMove state (0, -1)) xs
    | x == 'r' = move (applyMove state (0, 1)) xs
    | otherwise = move state xs

-- Devolve as dimensões do labirinto, (altura, largura)
mazeDimensions :: GameState -> (Int, Int)
mazeDimensions (GameState maze _ _) = (length maze, length $ head maze)

-- Devolve o caracter que está numa posição do labirinto
getCharInMaze :: [String] -> (Int, Int) -> Char
getCharInMaze maze (line, col) = maze !! line !! col


-- >>------------------------- [ Internal Functions ] -------------------------<<

-- Função auxilar do move para aplicar um movimento
applyMove :: GameState -> (Int, Int) -> GameState
applyMove (GameState maze pos keys) movDir
    | nextChar == '*' = unchangedState
    | nextChar == '@' = GameState maze (getCharPosInMaze (getMazeWithPlayer movedState) '@') keys
    | nextChar == ' ' || nextChar == 'S' = movedState
    | nextChar == 'F' = GameState maze nextPos keys
    | isLower nextChar = GameState (removeKeyFromMaze maze nextChar) nextPos (sort $ nub $ nextChar : keys)
    | otherwise = if toLower nextChar `elem` keys then
        GameState (removeKeyFromMaze maze nextChar) nextPos keys
        else unchangedState
    where
        nextPos = (fst pos + fst movDir, snd pos + snd movDir)
        nextChar = getCharInMaze maze nextPos
        unchangedState = GameState maze pos keys
        movedState = GameState maze nextPos keys

-- Remove caracter do labirinto e o substitui por um espaço (' ')
removeKeyFromMaze :: [String] -> Char -> [String]
removeKeyFromMaze maze key = map (map (\c -> if c == key then ' ' else c)) maze

-- A partir de um caracter retorna as cordenadas deste no labirinto
getCharPosInMaze :: [String] -> Char -> (Int, Int)
getCharPosInMaze maze c = getCharPosInMazeAux maze c 0
    where
        getCharPosInMazeAux :: [String] -> Char -> Int -> (Int, Int)
        getCharPosInMazeAux [] _ _ = (-1, -1)
        getCharPosInMazeAux (x:xs) c i = 
            case c `elemIndex` x of
                Just n -> (i, n)
                Nothing -> getCharPosInMazeAux xs c (i+1)