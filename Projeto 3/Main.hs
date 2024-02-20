{-
Princípios de Programação 2022/2023
Trabalho 4

Grupo
 - Rodrigo Correia - 58180
 - Daniel Nunes - 58257

$ stack ghc Main.hs

A instrução acima produz um executável Main, que deverá ser executável
através de um dos seguintes três tipos de instruções:

$ ./Main [ficheiro] -- carrega um ficheiro para jogar
$ ./Main            -- carrega o ficheiro default.map
$ ./Main -t         -- corre os testes
-}
module Main
(
    main
) where
import Labirintos ( move, GameState(playerPos, keys, maze) )
import System.Environment ( getArgs )
import System.Directory ( doesFileExist )
import Tests ( runAllTests )

-- Carrega um GameState de um ficheiro de mapa, devolve um Maybe GameState, retornando Nothing caso o ficheiro nao exista, e Just GameState
loadMapFile :: FilePath -> IO (Maybe GameState)
loadMapFile path = do
    fileExists <- doesFileExist path
    if fileExists then do
        fileContents <- readFile path
        return $ Just (read fileContents :: GameState)
    else
        return Nothing

-- Função de ordem superior que tenta carregar um GameSate, através do loadMapFile, caso esta retorne Nothing é executada a função failFunc, caso contrário
-- é executada a função successFunc
tryLoadMap :: FilePath -> (GameState -> IO ()) -> IO () -> IO ()
tryLoadMap path successFunc failFunc = do
    maybeState <- loadMapFile path
    case maybeState of 
        (Just state) -> successFunc state
        Nothing -> failFunc

-- Escreve num ficheiro de mapa o GameState atual
saveMapFile :: FilePath -> GameState -> IO ()
saveMapFile path gameState = do
    let serializedState = [show (playerPos gameState), keys gameState] ++ maze gameState
    writeFile path (unlines serializedState)

-- Inicia o jogo, esta função é chamada recursivamente com o GameSate atualizado até ser enviado o comando exit
playGame :: GameState -> IO ()
playGame gameState = do
    print gameState
    (command, arg) <- readGameCommand
    case command of
        "move" -> playGame (move gameState arg)
        "load" -> tryLoadMap arg playGame (do 
                    putStrLn $ "The map file '" ++ arg ++ "' does not exists!"
                    playGame gameState)
        "save" -> do
            saveMapFile arg gameState
            playGame gameState
        _ -> return ()

-- Lé o input do utilizador do terminal e verifica se é valida retornando o comando usado e o seu argumento
readGameCommand :: IO (String, String)
readGameCommand = do
    input <- getLine
    let (command:args) = words input
    if null args && command /= "exit" then do
        putStrLn "This command needs an argument to be executed!"
        readGameCommand
    else
        if command `notElem` validCommands then do
            putStrLn "Invalid command!"
            readGameCommand
        else if length args > 1 then do
            putStrLn "Use only one argument with the command"
            readGameCommand
        else
            return (command, head args)
    where
        validCommands = ["move", "load", "save", "exit"]

-- Inicia o programa e verifica os argumentos passados
-- nenhum argumento -> dá load do default.map
-- -t -> corre os testes
-- [filename] -> inicia o jogo nesse save file
main :: IO ()
main = do
    args <- getArgs
    let argc = length args
    if argc == 0 then
        tryLoadMap "default.map" playGame $ putStrLn $ "The map file 'default.map' does not exists!"
    else if argc == 1 then do
        let arg = head args
        case arg of
            "-t" -> runAllTests
            _ -> do
                tryLoadMap arg playGame $ putStrLn $ "The map file '" ++ arg ++ "' does not exists!"
    else do
        putStrLn "Invalid startup arguments! Run the program like this:"
        putStrLn "./Main [ficheiro] - carrega um ficheiro para jogar"
        putStrLn "./Main - carrega o ficheiro default.map"
        putStrLn "./Main -t - corre os testes"