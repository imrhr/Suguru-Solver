module Backtracking (Timeline (Null, Elem), newTL, goback, addelement) where

data Timeline = Null | Elem Timeline Matriz Int (Int, Int)
                deriving (Eq, Ord, Read, Show)

--Cria ponto de partida de nova Timeline
newTL :: Matriz -> Int -> (Int, Int) -> Timeline
newTL m v t = (Elem Null m v t)

--Retorna ao elemento anterior
goback :: Timeline -> Timeline
goback (Elem ant _ _ _) = ant
goback Null = Null

--Adiciona novo elemento a Timeline
addelement :: Timeline -> Matriz -> Int -> (Int, Int) -> Timeline
addelement tl m v tup = (Elem tl m v tup)
