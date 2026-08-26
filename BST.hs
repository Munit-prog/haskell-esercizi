{-# LANGUAGE DatatypeContexts #-}

-- definizione

data (Ord a, Show a, Read a) => BST a = Void | Node {
  val :: a,
  left, right :: BST a
 }
 deriving (Eq, Ord, Read, Show)

--somma dei valori di un albero a valori sommabili

somma_BST :: (Num a, Ord a, Show a, Read a)  => BST a -> a
somma_BST Void = 0
somma_BST (Node x sx dx) = somma_BST sx  + x + somma_BST dx

--somma dei valori dispari

somma_dispari :: (Integral a, Ord a, Show a, Read a) => BST a -> a
somma_dispari Void = 0
somma_dispari (Node x sx dx)
 | odd x = x + somma_dispari sx + somma_dispari dx
 | otherwise = somma_dispari sx + somma_dispari dx

--lista di alberi controlla se somme sono uguali

somma_listBST :: (Num a, Ord a, Show a, Read a) => [BST a] -> [a]
somma_listBST x = map somma_BST x

tutti_uguali :: Eq a => [a] -> Bool
tutti_uguali [] = True
tutti_uguali (x:xs)= all (==x) xs 

samesum :: (Num a, Ord a, Show a, Read a) => [BST a] -> Bool
samesum x = tutti_uguali (somma_listBST x)

--ricerca

bstElem :: (Ord a, Show a, Read a) => a -> BST a -> Bool
bstElem a Void = False
bstElem a (Node x sx dx) 
 | a == x = True
 | a > x = bstElem a dx
 |otherwise = bstElem a sx


--inserimento

insert :: (Ord a, Show a, Read a) => BST a -> a -> BST a
insert Void y = Node y Void Void
insert (Node x sx dx) y
 | y > x = Node x sx (insert dx y)
 | y < x = Node x (insert sx y) dx
 | otherwise = Node x sx dx

--lista ordinata da BST
bst2List :: (Ord a, Show a, Read a) => BST a -> [a]
bst2List x = bst2ListAux x []

bst2ListAux :: (Ord a, Show a, Read a) => BST a -> [a] ->[a]
bst2ListAux Void acc = acc
bst2ListAux (Node x sx dx) acc = bst2ListAux sx (x : bst2ListAux dx acc)


--ordinamento lista tramite BST
list2bst :: (Ord a, Show a, Read a) => [a] -> BST a
list2bst [] = Void
list2bst (x:xs) = insert (list2bst xs) x

bstSort :: (Ord a, Show a, Read a) => [a] -> [a]
bstSort xs = bst2List (list2bst xs)


--lista ordinata dopo un filter su albero
filtertree :: (Ord a, Show a, Read a) => (a -> Bool) -> BST a -> [a]
filtertree p t = filter p (bst2List t)


--costruire albero insieme alla sua altezza
altezza :: (Ord a, Show a, Read a) => BST a -> Int
altezza Void = -1
altezza (Node _ sx dx) = 1 + max (altezza sx) (altezza dx) 

annotate :: (Ord a, Show a, Read a) => BST a -> BST (a, Int)
annotate Void = Void
annotate t@(Node x sx dx) = (x, altezza t) (annotate sx) (annotate dx)


--altezza tra figli differenza massimo 1
almostBalanced :: (Ord a, Show a, Read a) => BST a -> Bool
almostBalanced Void = True
almostBalanced (Node _ sx dx) =
 abs (altezza sx - altezza dx) <= 1 && almostBalanced sx && almostBalanced dx


--Weighted Binary Search Tree, BST con altezza mantenuta nel nodo

data WBST a = Void | Node a Int (WBST a) (WBST a)

altezzaW :: WBST a -> Int
altezzaW void = -1
altezzaW (Node _ h _ _) = h

nodo :: a -> WBST a -> WBST a -> WBST a
nodo x sx dx = Node x (1 + max (altezzaW sx) (altezzaW dx)) sx dx

insertW :: Ord a => WBST a -> a -> WBST a
insertW void y = nodo y Void Void
insertW t@(Node x _ sx dx) y
 | y > x = nodo x sx (insertW dx y)
 | y < x = nodo x (insertW sx y) dx
 | otherwise = t

--differenza con il nodo successivo
minimo :: (Ord a, Show a, Read a) => BST a -> a
minimo (Node x Void _) = x
minimo (Node _ sx _) = minimo sx

diff2next :: (Num a, Ord a, Show a, Read a) => BST a -> BST (a, Maybe a)
diff2next t = aux t Nothing
 where
  aux Void _ = Void
  aux (Node x sx dx) s = Node (x, d) (aux sx (Just x)) (aux dx s)
   where succ_x = case dx of 
                     Void -> s
                     _    -> Just (minimo dx)
         d = case succ_x of
                Nothing -> Nothing
                Just y -> Just (y - x)
