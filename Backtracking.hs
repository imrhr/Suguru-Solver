module Backtracking (Timeline (Null, Elem), newTL, goback, addelement, returnMatrix, Lista, Matriz) where

type Lista = [Int]
type Matriz = [Lista]
data Timeline = Null | Elem Timeline Matriz Matriz Int (Int, Int)
                deriving (Eq, Ord, Read, Show)

--Cria ponto de partida de nova Timeline
newTL :: Matriz -> Matriz -> Timeline
newTL mP mA = (Elem Null mP mA 1 (0, 0))

--Retorna ao elemento anterior
goback :: Timeline -> Timeline
goback (Elem (Elem a mP mA v tup) _ _ _ _) = (Elem a mP mA (v + 1) tup)
goback Null = Null

--Adiciona novo elemento a Timeline
addelement :: Timeline -> Matriz -> Matriz -> (Int, Int) -> Timeline
addelement tl mP mA tup = (Elem tl mP mA 1 tup)

returnMatrix :: Timeline -> Matriz
returnMatrix (Elem _ m _ _ _) = m
returnMatrix Null = []
