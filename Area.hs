module Area (Lista, Matriz, counter, tamArea) where

type Lista = [Int]
type Matriz = [Lista]

--Contador para o tamanho da área
counter :: Lista -> Int -> Int
counter [] _ = 0
counter (a:b) x | (x == a) = 1 + counter b x
                | otherwise = 0 + counter b x

--Mede o tamanho de uma área
tamArea :: Matriz -> Int -> Int
tamArea [] _ = 0
tamArea (a:b) x = counter a x + (tamArea b x)
