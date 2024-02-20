{- Podem usar esta plataforma para testar o vosso código. Atenção que:
   1. esta plataforma NÃO substitui a submissão do vosso trabalho, que deverá
      ser feita pelo Moodle
   2. os testes aqui implementados poderão não ser idênticos aos testes
      utilizados para avaliação final
-}

-- Funcao que devolve a posicao inicial do labirinto
posicaoInicial :: [String] -> (Int, Int)
posicaoInicial xs = findIndex xs 'S'

-----------

--Funcao que devolve os vizinhos de uma cordenada se esses existirem
vizinhos :: [String] -> (Int, Int) -> [(Int, Int)]
vizinhos xs y = verifica xs [(fst y - 1, snd y), (fst y + 1, snd y), (fst y, snd y -1 ), (fst y, snd y + 1)]

--funcao auxiliar
verifica :: [String] -> [(Int, Int)] -> [(Int, Int)]
verifica _ [] = []
verifica xs (y:ys)
   | achaLinha xs y 0 = y:verifica xs ys
   | otherwise = verifica xs ys

--Funcao que a partir do indice da cordenada acha a linha que este esta presente
achaLinha :: [String] -> (Int, Int) -> Int -> Bool
achaLinha [] _ _ = False
achaLinha (x:xs) y index
   | index == fst y = achaColuna x (snd y) 0
   | otherwise = achaLinha xs y (index+1)

--Funcao que a partir do indice da cordenada acha a coluna que este esta presente e verifica se e uma posicao possivel
achaColuna :: String -> Int -> Int -> Bool
achaColuna [] _ _ = False
achaColuna (x:xs) y index
   | index == y = x == ' ' || x == 'S' || x == 'F'
   | otherwise = achaColuna xs y (index+1)

----------

--funcao que verifica se existe um caminho iniciando pelo "S" e indo ate o "F"
procuraCaminho :: [String] -> Bool
procuraCaminho xs = verificaCaminho xs (criaCaminho xs [] [inicio])
   where inicio = posicaoInicial xs

--Funcao que recebe um caminho e verifica se esse tem acesso ao final "F"
verificaCaminho :: [String] -> [(Int, Int)] -> Bool
verificaCaminho xs = foldr (\y acc -> y == posFinal || acc) False
   where posFinal = findIndex xs 'F'

--funcao que cria caminho possivel a partir dos vizihos das coordenadas
criaCaminho :: [String] -> [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
criaCaminho _ _ [] = []
criaCaminho xs check (y:ys)
   | pertence check y = criaCaminho xs check ys
   | otherwise = y:criaCaminho xs (y:check) (ys ++ vizinhos xs y)

--Funcao que verifica se uma coordenada existe na lista
pertence :: [(Int,Int)] -> (Int, Int) -> Bool
pertence xs y = foldr (\x acc -> y `elem` xs) False xs

------------------- Aux

--funcao que devolve as coordenadas de um certo caracter no labirinto, esse podendo ser "S" ou "F"
findIndex :: [String] -> Char -> (Int, Int)
findIndex [] _ = (-6,-9)
findIndex (x:xs) c
   | findIndexAux x c 0 > 0 = (length x - length xs - 1, findIndexAux x c 0)
   | otherwise  = findIndex xs c

--Funcao auxiliar
findIndexAux :: String -> Char -> Int -> Int
findIndexAux [] _ _ = -1
findIndexAux (x:xs) c index
   |  x == c = index
   | otherwise = findIndexAux xs c (index + 1)