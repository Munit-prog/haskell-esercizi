
data (Eq a, Show a) => QT a = C a | Q (QT a) (QT a) (QT a) (QT a)
 deriving (Eq, Show)


--buildNSimplify

buildNSimplify :: (Eq a, Show a) => QT a -> QT a -> QT a -> QT a -> QT a
buildNSimplify (C c1) (C c2) (C c3) (C c4)
  | c1 == c2 && c2 == c3 && c3 == c4 = C c1
buildNSimplify t1 t2 t3 t4 = Q t1 t2 t3 t4

--simplify da un QT genera quadtree

simplify :: (Eq a, Show a) => QT a -> QT a
simplify ( C c) = C c
simplify (Q t1 t2 t3 t4) = buildNSimplify (simplify t1) (simplify t2) (simplify t3) (simplify t4)

--map

mapQT :: (Eq a, Show a, Eq b, Show b) => (a -> b) -> QT a -> QT b
mapQT f (C c) = C (f c)
mapQT f (Q t1 t2 t3 t4) = buildNSimplify (mapQT f t1) (mapQT f t2) (mapQT f t3) (mapQT f t4)

--funzione che determina numero minimo pixel dell'immagine

howManyPixels :: (Eq a, Show a) => QT a -> Int
howManyPixels (C _) = 1
howManyPixels (Q t1 t2 t3 t4) = 4 * max (max n1 n2) (max n3 n4)
 where n1 = howManyPixels t1
       n2 = howManyPixels t2
       n3 = howManyPixels t3
       n4 = howManyPixels t4

--limitAll: colori

limit :: (Ord a, Show a) => a -> QT a -> QT a
limit c q = mapQT (\x -> min x c) 1

limitAll :: (Ord a, Show a) => a -> [QT a] -> [QT a]
limitAll c [] = []
limitAll c (q:qs) = limit c q : limitAll c qs


--funzione determina numero pixel di quel colore

occurrencies :: (Eq a, Show a) => QT a -> a -> Int
occurencies q c = count q (hieght q)
 where count (C x) k
           | x == c = 4 ^ k
           | otherwise = 0
       count (Q t1 t2 t3 t4) k = count t1 (k-1) + count t2 (k-1) + count t3 (k-1) + count t4 (k-1)


--differenza tra colore e qt

difference :: (Ord a, Show a) => a -> QT a -> Int
difference c q = go q (height q)
 where go (C x) k
       | x > c = 4 ^ k
       | x < c = -(4 ^ k)
       | otherwise = 0
     go (Q t1 t2 t3 t4) k = go t1 (k-1) + go t2 (k-1) + go t3 (k-1) + go t4 (k-1)

--colore maggiore

overColor :: (Ord a, Show a) => a -> Qt a -> Int
overColor c q = go q (height q)
 wheere go (C x) k
       | x > c = 4 ^ k
       | otherwise = 0
      go (Q t1 t2 t3 t4) k = go t1 (k-1) + go t2 (k-1) + go t3 (k-1) + go t4 (k-1)


