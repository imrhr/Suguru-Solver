module Backtracking (Timeline(Teste), espacoembranco, geraResultado, backtrack) where

import ManipulaMatrix
import Verificador
import Area

data Timeline = Teste Int (Int, Int) Matriz Matriz Lista

espacoembranco :: Matriz -> (Int, Int) -> (Int, Int)
espacoembranco m (0, 0) = (-1, -1)

espacoembranco m (x, 0) | (((m!!(x - 1))!!((compMatrix m) - 1)) == 0) = ((x - 1), ((compMatrix m) - 1))
                        | otherwise = espacoembranco m ((x - 1), ((compMatrix m) - 1))

espacoembranco m (0, y) | (((m!!0)!!(y - 1)) == 0) = (0, (y - 1))
                        | otherwise = espacoembranco m (0, (y - 1))

espacoembranco m (x, y) | (((m!!x)!!(y - 1)) == 0) = (x, (y - 1))
                        | otherwise = espacoembranco m (x, (y - 1))

geraResultado :: Matriz -> Lista -> (Int, Int) -> Matriz
geraResultado mp [] _ = mp
geraResultado mp (a:b) (x,y) | ((y == ((compMatrix mp) - 1)) && (((mp!!x)!!y) == 0)) = geraResultado (swapMatrix mp x y a) b (x + 1, 0)
                             | (((mp!!x)!!y) == 0) = geraResultado (swapMatrix mp x y a) b (x, y + 1)
                             | ((y == ((compMatrix mp) - 1)) && (((mp!!x)!!y) /= 0)) = geraResultado mp (a:b) (x + 1, 0)
                             | otherwise = (geraResultado mp (a:b) (x, (y + 1)))

backtrack :: Timeline -> Maybe Matriz
backtrack (Teste v pos mp ma s) = do
  if (((((fst pos) > ((compMatrix mp) - 1)) && ((snd pos) > ((compMatrix mp)-1))) || ((fst pos) < 0)) || ((snd pos) < 0)) then
    Just (geraResultado mp (reverse s) (0, 0))
  else
    if ( ((mp!!(fst pos))!!(snd pos)) /= 0) then
      if (((snd pos)+1) < (compMatrix mp)) then
        backtrack (Teste 1 ((fst pos), ((snd pos)+1) ) mp ma s)
      else
          if (((fst pos)+1) >= (compMatrix mp)) then
              backtrack (Teste 1 (((fst pos)+1), ((snd pos)+1) ) mp ma s)
          else
              backtrack (Teste 1 (((fst pos)+1), 0) mp ma s)
    else
      if (v > (tamArea ma ((ma!!(fst pos))!!(snd pos)))) then
        backtrack (Teste ((head s) + 1) (espacoembranco mp pos) mp ma (tail s))
      else
        if (verify (geraResultado mp (reverse s) (0,0)) ma v pos) then
            if (((snd pos)+1) < (compMatrix mp)) then
                backtrack (Teste 1 ((fst pos), ((snd pos)+1) ) mp ma (v:s))
            else
                if (((fst pos)+1) >= (compMatrix mp)) then
                    backtrack (Teste 1 (((fst pos)+1), ((snd pos)+1) ) mp ma (v:s))
                else
                    backtrack (Teste 1 (((fst pos)+1), 0) mp ma (v:s))
        else
          backtrack (Teste (v + 1) pos mp ma s)