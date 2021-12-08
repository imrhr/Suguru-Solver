module Verificador (compMatrix,comparaArea, diagonal, linha, coluna, comparaRedor, verify) where

import ManipulaMatrix (Lista, Matriz)

--Tamanho da Matriz
compMatrix :: Matriz -> Int
compMatrix [] = 0
compMatrix (_:b) = 1 + (compMatrix b)

--Procura elementos iguais na mesma área
comparaArea :: Matriz -> Matriz -> Int -> Int -> Int -> Int -> Bool
comparaArea mP mA x y a v | ((((((mA!!x)!!y) == a) && (((mP!!x)!!y) /= v)) && (y == ((compMatrix mP) - 1))) && (x == ((compMatrix mP) - 1))) = True
                          | ((((((mA!!x)!!y) == a) && (((mP!!x)!!y) /= v))) && (y == ((compMatrix mP) - 1))) = (True && comparaArea mP mA (x + 1) 0 a v)
                          | (((((mA!!x)!!y) == a) && (((mP!!x)!!y) /= v))) = (True && comparaArea mP mA x (y + 1) a v)
                          | (((((mA!!x)!!y) /= a) && (y == ((compMatrix mP) - 1))) && (x == ((compMatrix mP) - 1))) = True
                          | ((((mA!!x)!!y) /= a) && (y == ((compMatrix mP) - 1))) = (True && comparaArea mP mA (x + 1) 0 a v)
                          | (((mA!!x)!!y) /= a) = (True && comparaArea mP mA x (y + 1) a v)
                          | otherwise = False

--Procura elementos iguais na diagonal
diagonal :: Matriz -> Int -> Int -> Bool
diagonal m x y | ((x == 0) && (y == 0)) = (((m!!x)!!y) /= ((m!!(x + 1))!!(y + 1)))
               | ((x == 0) && (y /= 0)) = ((((m!!x)!!y) /= ((m!!(x + 1))!!(y + 1))) && (((m!!x)!!y) /= ((m!!(x + 1))!!(y - 1))))
               | ((x /= 0) && (y == 0)) = ((((m!!x)!!y) /= ((m!!(x + 1))!!(y + 1))) && (((m!!x)!!y) /= ((m!!(x - 1))!!(y + 1))))
               | ((x == ((compMatrix m) - 1)) && (y == ((compMatrix m) - 1))) = (((m!!x)!!y) /= ((m!!(x - 1))!!(y - 1)))
               | ((x == ((compMatrix m) - 1)) && (y /= ((compMatrix m) - 1))) = ((((m!!x)!!y) /= ((m!!(x - 1))!!(y + 1))) && (((m!!x)!!y) /= ((m!!(x - 1))!!(y - 1))))
               | ((x /= ((compMatrix m) - 1)) && (y == ((compMatrix m) - 1))) = ((((m!!x)!!y) /= ((m!!(x - 1))!!(y - 1))) && (((m!!x)!!y) /= ((m!!(x + 1))!!(y - 1))))
               | otherwise = (((((m!!x)!!y) /= ((m!!(x + 1))!!(y + 1))) && (((m!!x)!!y) /= ((m!!(x - 1))!!(y - 1)))) && ((((m!!x)!!y) /= ((m!!(x - 1))!!(y + 1))) &&
                             (((m!!x)!!y) /= ((m!!(x + 1))!!(y - 1)))))

--Procura elementos iguais na coluna
coluna :: Matriz -> Int -> Int -> Bool
coluna m x y | (y == 0) = ((m!!x)!!y) /= ((m!!x)!!(y + 1))
             | (y == ((compMatrix m) - 1)) = (((m!!x)!!y) /= ((m!!x)!!(y - 1)))
             | otherwise = ((((m!!x)!!y) /= ((m!!x)!!(y +  1))) && (((m!!x)!!y) /= ((m!!x)!!(y - 1))))

--Procura elementos iguais na linha
linha :: Matriz -> Int -> Int -> Bool
linha m x y | (x == 0) = ((m!!x)!!y) /= ((m!!(x + 1))!!y)
            | (x == ((compMatrix m) - 1)) = ((m!!x)!!y) /= ((m!!(x - 1))!!y)
            | otherwise = ((((m!!x)!!y) /= ((m!!(x + 1))!!y)) && (((m!!x)!!y) /= ((m!!(x - 1))!!y)))

--Procura elementos iguais ao redor do elemento
comparaRedor :: Matriz -> Int -> Int -> Bool
comparaRedor m x y = ((linha m x y && coluna m x y) && diagonal m x y)

verify :: Matriz -> Matriz -> Int -> (Int, Int) -> Bool
verify mP mA v (x, y) = ((comparaRedor mP x y) && (comparaArea mP mA x y v ((mA!!x)!!y)))
