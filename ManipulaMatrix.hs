module ManipulaMatrix (insert, swap, Lista, Matriz) where
    type Lista = [Int]    
    type Matriz = [Lista]
    --Insere elemento entre duas listas
    insert :: (Lista, Lista) -> Int -> Lista
    insert (l1, (a:b)) v = (l1 ++ v : b)

    --Troca elemento da linha da matriz
    swap :: Lista -> Int -> Int -> Lista
    swap l y v = insert (splitAt y l) v