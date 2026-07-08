reverse1 [] = []
reverse1 (x:xs) = reverse1 xs ++ [x]

-- reverseAux xs ys = reveserse xs ++ ys 

reverseAux [] ys = ys
reverseAux (x : xs) ys = reverseAux xs (x : ys)

reverse2 xs = foldl (\ ys y -> y: ys ) [] xs

reverse3 xs = foldr (\ y ys -> ys ++ [y] ) [] xs

addPari [] = 0
addPari (x : xs) | (mod x 2) == 0 = x + addPari xs
                 | otherwise      = addPari xs

addPariAux [] acc = acc
addPariAux (x : xs) acc | (mod x 2) == 0 = addPariAux xs (acc + x)
                        | otherwise      = addPariAux xs acc

addPari2 xs = addPariAux xs 0

addPari3 xs = foldl (\ acc x -> case (mod x 2) of 0 -> x + acc
                                                  otherwise -> acc) 0 xs
addPari4 xs = foldr (\ x acc -> case (mod x 2) of 0 -> x + acc
                                                  otherwise -> acc) 0 xs

map2 f [] = []
map2 f (x : xs) = f x : map2 f xs

mapAux f [] ys = ys
mapAux f (x : xs) ys  = mapAux f xs (f x : ys)

map3 f xs = reverse (mapAux f xs [])

map4 f xs = [ f x | x  <- xs]

map5 f xs = foldr ( \ x ys -> f x : ys) [] xs

map5b f = foldr ( \ x ys -> f x : ys) []

map6 f xs = reverse ( foldl ( \ ys x -> f x : ys) []  xs)

data BTree a = Null | Branch a (BTree a) (BTree a)

sumBTree Null = 0
sumBTree (Branch x t1 t2) = x + sumBTree t1 + sumBTree t2

depthBTree Null = 0
depthBTree (Branch _ t1 t2) = 1 + max (depthBTree t1) (depthBTree t2)

takeBTree _ 0 = Null
takeBTree Null _ = Null
takeBTree (Branch x t1 t2) n = Branch x (takeBTree t1 (n-1)) (takeBTree t2 (n-1))

insertBTree Null x = Branch x Null Null
insertBTree (Branch y t1 t2) x | x < y = Branch y (insertBTree t1 x) t2
insertBTree (Branch y t1 t2) x | otherwise = Branch y t1 (insertBTree t2 x)

toListBTree Null = []
toListBTree (Branch y t1 t2) = toListBTree t1 ++  (y : toListBTree t2)

toListBTreeAux Null acc = acc
toListBTreeAux (Branch y t1 t2) acc = toListBTreeAux t1 (y : toListBTreeAux t2 acc)

order xs = toListBTree (foldl insertBTree Null xs)

x :: Float
x = fromInteger 3:
