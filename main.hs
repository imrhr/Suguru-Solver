module Main (main) where

import Backtracking

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
    print(espacoembranco mp (4, 5))
    print(geraResultado mp [1..29] (0, 0))
