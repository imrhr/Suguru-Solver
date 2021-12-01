type Lista = [Int]
type Matriz = [Lista]

compMatrix :: Matriz -> Int
compMatrix [] = 0
compMatrix (_:b) = 1 + (compMatrix b)

--comparaArea :: Matriz -> Matriz -> Int -> Int -> Bool

diagonal :: Matriz -> Int -> Int -> Bool
diagonal m x y | ((x == 0) && (y == 0)) = (((m!!x)!!y) /= ((m!!(x + 1))!!(y + 1)))
               | ((x == 0) && (y /= 0)) = ((((m!!x)!!y) /= ((m!!(x + 1))!!(y + 1))) && (((m!!x)!!y) /= ((m!!(x + 1))!!(y - 1))))
               | ((x /= 0) && (y == 0)) = ((((m!!x)!!y) /= ((m!!(x + 1))!!(y + 1))) && (((m!!x)!!y) /= ((m!!(x - 1))!!(y + 1))))
               | ((x == ((compMatrix m) - 1)) && (y == ((compMatrix m) - 1))) = (((m!!x)!!y) /= ((m!!(x - 1))!!(y - 1)))
               | ((x == ((compMatrix m) - 1)) && (y /= ((compMatrix m) - 1))) = ((((m!!x)!!y) /= ((m!!(x - 1))!!(y + 1))) && (((m!!x)!!y) /= ((m!!(x - 1))!!(y - 1))))
               | ((x /= ((compMatrix m) - 1)) && (y == ((compMatrix m) - 1))) = ((((m!!x)!!y) /= ((m!!(x - 1))!!(y - 1))) && (((m!!x)!!y) /= ((m!!(x + 1))!!(y - 1))))
               | otherwise = (((((m!!x)!!y) /= ((m!!(x + 1))!!(y + 1))) && (((m!!x)!!y) /= ((m!!(x - 1))!!(y - 1)))) && ((((m!!x)!!y) /= ((m!!(x - 1))!!(y + 1))) &&
                             (((m!!x)!!y) /= ((m!!(x + 1))!!(y - 1)))))

coluna :: Matriz -> Int -> Int -> Bool
coluna m x y | (y == 0) = ((m!!x)!!y) /= ((m!!x)!!(y + 1))
             | (y == ((compMatrix m) - 1)) = (((m!!x)!!y) /= ((m!!x)!!(y - 1)))
             | otherwise = ((((m!!x)!!y) /= ((m!!x)!!(y +  1))) && (((m!!x)!!y) /= ((m!!x)!!(y - 1))))

linha :: Matriz -> Int -> Int -> Bool
linha m x y | (x == 0) = ((m!!x)!!y) /= ((m!!(x + 1))!!y)
            | (x == ((compMatrix m) - 1)) = ((m!!x)!!y) /= ((m!!(x - 1))!!y)
            | otherwise = ((((m!!x)!!y) /= ((m!!(x + 1))!!y)) && (((m!!x)!!y) /= ((m!!(x - 1))!!y)))

comparaRedor :: Matriz -> Int -> Int -> Bool
comparaRedor m x y = ((linha m x y && coluna m x y) && diagonal m x y)

--limite :: Matriz -> Matriz -> Int -> Int -> Bool

counter :: Lista -> Int -> Int
counter [] _ = 0
counter (a:b) x | (x == a) = 1 + counter b x
                | otherwise = 0 + counter b x

tamArea :: Matriz -> Int -> Int
tamArea [] _ = 0
tamArea (a:b) x = counter a x + (tamArea b x)

main = do
    let mp = [[4,0,0,0,0,0],
              [0,0,0,0,0,0],
              [0,0,4,0,0,1],
              [0,0,0,2,0,0],
              [5,0,0,3,5,0],
              [0,0,0,0,0,0]]
    let ma = [[1,1,2,3,3,3],
              [1,2,2,2,3,4],
              [1,5,2,6,3,4],
              [5,5,6,6,6,4],
              [5,7,7,6,8,4],
              [5,8,8,8,8,4]]
    let mt = [[1,1,0,0,0,0],
              [0,0,0,2,0,0],
              [0,3,0,2,0,0],
              [0,0,3,0,0,0]]
    print(comparaRedor mt 2 1)
