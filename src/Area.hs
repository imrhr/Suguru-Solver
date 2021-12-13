module Area (counter, tamArea) where

import ManipulaMatrix (Lista, Matriz)

--Contador para o tamArea
counter :: Lista -> Int -> Int
counter [] _ = 0
counter (a:b) x | (x == a) = 1 + counter b x
                | otherwise = 0 + counter b x

--Mede o tamanho de uma área
tamArea :: Matriz -> Int -> Int
tamArea [] _ = 0
tamArea (a:b) x = counter a x + (tamArea b x)
