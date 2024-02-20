module Root.Src.T1_58257 where

{- Podem usar esta plataforma para testar o vosso código. Atenção que:
   1. esta plataforma NÃO substitui a submissão do vosso trabalho, que deverá
      ser feita pelo Moodle
   2. os testes aqui implementados poderão não ser idênticos aos testes
      utilizados para avaliação final
-}

contaCaracteres :: Char -> [String] -> Int
contaCaracteres c ls = conta c ((unlines ls))

conta :: Char -> String -> Int
conta c xs = length (conta2 c xs) 

conta2 :: Char -> String -> [Char]
conta2 c xs = [x | x <- xs, x == c, x /= '\n']

labirintos5 :: [[String]]
labirintos5 = verify

--verifica e filtra os labirintos validos
verify :: [[String]]
verify = [xs | xs <- create, contaCaracteres 'S' xs == 1 && contaCaracteres 'F' xs == 1]  

--Cria todas as combinaçoes de labirintos cercados por paredes
create :: [[String]]
create = [["*****", x, y, z, "*****"] | x <- linesLab, y <- linesLab, z <- linesLab]

--Cria todas as combinaçoes de linhas com paredes nas pontas
linesLab :: [String]
linesLab = ["*" ++ x ++ y ++ z ++ "*" | x <- charLab, y <- charLab, z <- charLab]
   where charLab = [" ","S","*","F"]




