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
