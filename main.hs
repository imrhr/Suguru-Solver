module Main (main) where

import Backtracking

main = do
    {--
    Matriz principal, contém os valores iniciais, espaços em branco representados
    por zero
    --}
    let mp = [[ 3, 0, 0, 3, 1, 2, 0],
              [ 0, 6, 0, 4, 0, 0, 3],
              [ 2, 0, 0, 0, 3, 2, 0],
              [ 0, 5, 0, 0, 0, 0, 0],
              [ 3, 0, 4, 0, 0, 4, 0],
              [ 0, 0, 1, 5, 1, 0, 1],
              [ 1, 0, 0, 2, 0, 3, 4]]
    --Matriz área, contém os grupos, cada um representado por um número
    let ma = [[ 1, 1, 2, 2, 2, 5, 5],
              [ 1, 1, 3, 4, 4, 6, 5],
              [ 1, 1, 4, 4, 8, 6, 6],
              [ 7, 4, 4, 8, 8, 9, 6],
              [ 7, 7, 8, 8, 9, 9,12],
              [10,10,10, 9, 9, 9,12],
              [11,10,10,12,12,12,12]]
    print(backtrack (Teste 1 (0, 0) mp ma []))
