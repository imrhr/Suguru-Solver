module Suguro (Lista, Matriz, compMatrix,comparaArea,comparaRedor,insert,
             swap, diagonal, linha, coluna, counter, tamArea) where

    type Lista = [Int]
    type Matriz = [Lista]

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

    --Contador para o tamanho da área
    counter :: Lista -> Int -> Int
    counter [] _ = 0
    counter (a:b) x | (x == a) = 1 + counter b x
                    | otherwise = 0 + counter b x

    --Mede o tamanho de uma área
    tamArea :: Matriz -> Int -> Int
    tamArea [] _ = 0
    tamArea (a:b) x = counter a x + (tamArea b x)

    --Insere elemento entre duas listas
    insert :: (Lista, Lista) -> Int -> Lista
    insert (l1, (a:b)) v = (l1 ++ v : b)

    --Troca elemento da linha da matriz
    swap :: Lista -> Int -> Int -> Lista
    swap l y v = insert (splitAt y l) v

    