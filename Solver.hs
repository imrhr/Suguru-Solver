module Solver (resolve, iniciar) where

import Area hiding (Matriz, Lista)
import Backtracking
import Verificador hiding (Matriz, Lista)
import ManipulaMatrix hiding (Matriz, Lista)

resolve :: Timeline -> Timeline
resolve (Elem ant mP mA v (x, y)) | (((((mP!!x)!!y) /= 0) && (y == ((compMatrix mP) - 1))) && (x == ((compMatrix mP) - 1))) = (addelement (Elem ant mP mA v (x, y)) mP mA (x, y))
                                  | ((((mP!!x)!!y) /= 0) && (y == ((compMatrix mP) - 1))) = (resolve (addelement (Elem ant mP mA v (x, y)) mP mA (x + 1, 0)))
                                  | (((mP!!x)!!y) /= 0) = resolve (addelement (Elem ant mP mA v (x, y)) mP mA (x, y + 1))
                                  | ((((verify mP mA v (x, y)) == True) && (y == ((compMatrix mP) - 1))) && (x == ((compMatrix mP) - 1))) = (addelement
                                  (Elem ant mP mA v (x, y)) (swapMatrix mP x y v) mA (x, y))
                                  | (((verify mP mA v (x, y)) == True) && (y == ((compMatrix mP) - 1))) = (resolve ((addelement (Elem ant mP mA v (x, y))
                                  (swapMatrix mP x y v) mA ((x + 1), 0))))
                                  | ((verify mP mA v (x, y)) == True) = (resolve (addelement (Elem ant mP mA v (x, y)) (swapMatrix mP x y v) mA (x, y + 1)))
                                  | ((verify mP mA v (x, y)) == False) && (v > tamArea mP ((mA!!x)!!y)) = (resolve (goback (Elem ant mP mA v (x, y))))
                                  | otherwise = (resolve (Elem ant mP mA (v + 1) (x, y)))
resolve Null = Null

iniciar :: Matriz -> Matriz -> Matriz
iniciar mp ma = (returnMatrix (resolve (newTL mp ma)))
