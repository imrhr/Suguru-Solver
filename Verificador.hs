module Verificador (compMatrix,comparaArea, diagonal, linha, coluna, comparaRedor, verify) where

import ManipulaMatrix (Lista, Matriz)

--Tamanho da Matriz
compMatrix :: Matriz -> Int
compMatrix [] = 0
compMatrix (_:b) = 1 + (compMatrix b)

--Procura elementos iguais na mesma área
comparaArea :: Matriz -> Matriz -> Int -> Int -> Int -> Int -> Bool
comparaArea mP mA x y a v | ((((((mA!!x)!!y) == a) && (((mP!!x)!!y) /= v)) && (y == ((compMatrix mP) - 1))) && (x == ((compMatrix mP) - 1))) = True
                          | ((((((mA!!x)!!y) == a) && (((mP!!x)!!y) /= v))) && (y == ((compMatrix mP) - 1))) = (comparaArea mP mA (x + 1) 0 a v)
                          | (((((mA!!x)!!y) == a) && (((mP!!x)!!y) /= v))) = (comparaArea mP mA x (y + 1) a v)
                          | (((((mA!!x)!!y) /= a) && (y == ((compMatrix mP) - 1))) && (x == ((compMatrix mP) - 1))) = True
                          | ((((mA!!x)!!y) /= a) && (y == ((compMatrix mP) - 1))) = (comparaArea mP mA (x + 1) 0 a v)
                          | (((mA!!x)!!y) /= a) = (comparaArea mP mA x (y + 1) a v)
                          | otherwise = False

--Procura elementos iguais na diagonal
diagonal :: Matriz -> Int -> Int -> Int -> Bool
diagonal m x y v | ((x == 0) && (y == 0)) = (v /= ((m!!(x + 1))!!(y + 1)))
               | ((x == 0) && (y == ((compMatrix m) - 1))) = (v /= ((m!!(x + 1))!!(y - 1)))
               | ((x == ((compMatrix m) - 1)) && (y == 0)) = (v /= ((m!!(x - 1))!!(y + 1)))
               | ((x == 0) && (y > 0)) = ((v /= ((m!!(x + 1))!!(y + 1))) && (v /= ((m!!(x + 1))!!(y - 1))))
               | ((x > 0) && (y == 0)) = ((v /= ((m!!(x + 1))!!(y + 1))) && (v /= ((m!!(x - 1))!!(y + 1))))
               | ((x == ((compMatrix m) - 1)) && (y == ((compMatrix m) - 1))) = (v /= ((m!!(x - 1))!!(y - 1)))
               | ((x == ((compMatrix m) - 1)) && (y < ((compMatrix m) - 1))) = ((v /= ((m!!(x - 1))!!(y + 1))) && (v /= ((m!!(x - 1))!!(y - 1))))
               | ((x < ((compMatrix m) - 1)) && (y == ((compMatrix m) - 1))) = ((v /= ((m!!(x - 1))!!(y - 1))) && (v /= ((m!!(x + 1))!!(y - 1))))
               | otherwise = (((v /= ((m!!(x + 1))!!(y + 1))) && (v /= ((m!!(x - 1))!!(y - 1)))) && ((v /= ((m!!(x - 1))!!(y + 1))) &&
                             (v /= ((m!!(x + 1))!!(y - 1)))))

--Procura elementos iguais na coluna
linha :: Matriz -> Int -> Int -> Int -> Bool
linha m x y v| (y == 0) = (v /= ((m!!x)!!(y + 1)))
            | (y == ((compMatrix m) - 1)) = (v /= ((m!!x)!!(y - 1)))
            | otherwise = ((v /= ((m!!x)!!(y +  1))) && (v /= ((m!!x)!!(y - 1))))

--Procura elementos iguais na linha
coluna :: Matriz -> Int -> Int -> Int -> Bool
coluna m x y v | (x == 0) = (v /= ((m!!(x + 1))!!y))
               | (x == ((compMatrix m) - 1)) = (v /= ((m!!(x - 1))!!y))
               | otherwise = ((v /= ((m!!(x + 1))!!y)) && (v /= ((m!!(x - 1))!!y)))

--Procura elementos iguais ao redor do elemento
comparaRedor :: Matriz -> Int -> Int -> Int -> Bool
comparaRedor m x y v = ((linha m x y v && coluna m x y v) && diagonal m x y v)

verify :: Matriz -> Matriz -> Int -> (Int, Int) -> Bool
verify mP mA v (x, y) = ((comparaRedor mP x y v) && (comparaArea mP mA 0 0 ((mA!!x)!!y) v))
