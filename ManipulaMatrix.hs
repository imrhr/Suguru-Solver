module ManipulaMatrix (insertList, swapList, insertMatrix, swapMatrix, Lista, Matriz) where

type Lista = [Int]
type Matriz = [Lista]

--Insere elemento entre duas listas
insertList :: (Lista, Lista) -> Int -> Lista
insertList (l1, (a:b)) v = (l1 ++ v : b)

--Troca elemento da linha da matriz
swapList :: Lista -> Int -> Int -> Lista
swapList l y v = insertList (splitAt y l) v

insertMatrix :: (Matriz, Matriz) -> Int -> Int -> Matriz
insertMatrix (m1, (a:b)) v y = (m1 ++ (swapList a y v) : b)

--Troca Linha da matriz
swapMatrix :: Matriz -> Int -> Int -> Int -> Matriz
swapMatrix m x y v = insertMatrix (splitAt x m) v y
