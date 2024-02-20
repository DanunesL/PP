{-
Princípios de Programação 2022/2023
Trabalho 4

Grupo
 - Rodrigo Correia - 58180
 - Daniel Nunes - 58257
 -}
module Utils
(
    replaceCharAtIndex,
    replaceCharInStringList,
    chooseFromList
) where
import Test.QuickCheck ( Gen, choose )

-- Troca um caracter por outro, num  dado indice numa string
replaceCharAtIndex :: String -> Int -> Char -> String 
replaceCharAtIndex str i c = snd $ 
    foldl (\(counter, result) x -> if counter == i 
                                    then (counter + 1, result ++ [c]) 
                                    else (counter + 1, result ++ [x])) (0, "") str

-- Troca um caracter por outro, numa dada posicao, numa lista de strings
replaceCharInStringList :: [String] -> (Int, Int) -> Char -> [String]
replaceCharInStringList xs (a, b) c = snd $
    foldl (\(counter, result) x -> if counter == a 
                                    then (counter + 1, result ++ [replaceCharAtIndex x b c]) 
                                    else (counter + 1, result ++ [x])) (0, []) xs

-- Seleciona um elemento aleatorio de uma lista
chooseFromList :: [a] -> Gen a
chooseFromList [] = error "Empty list!"
chooseFromList list = do
    index <- choose (0, length list - 1) :: Gen Int
    return $ list !! index