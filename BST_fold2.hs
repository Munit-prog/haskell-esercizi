

--definizione

data (Ord a, Show a, Read a) => BST a = Void | Node
 {
 val :: a,
 left,
 right :: BST a
 }
 deriving (Eq, Ord, Read, Show)

fold :: (Ord a, Show a, Read a) => (a -> b -> b -> b) -> b -> BST a -> b
fold _ z Void           = z
fold f z (Node x sx dx) = f x (fold f z sx) (fold f z dx)

--bst2List

bst2ListAux Void           = \acc -> acc
bst2ListAux (Node x sx dx) = \acc -> bst2ListAux sx (x : bst2ListAux dx acc)

bst2List :: (Ord a, Show a, Read a) => BST a -> [a]
bst2List t = fold (\x l r acc -> l (x : r acc)) id t []

--filterTree

filtertreeAux p Void           acc = acc
filtertreeAux p (Node x sx dx) acc
  | p x       = filtertreeAux p sx (x : filtertreeAux p dx acc)
  | otherwise = filtertreeAux p sx     (filtertreeAux p dx acc)

filtertree :: (Ord a, Show a, Read a) => (a -> Bool) -> BST a -> [a]
filtertree p t = fold (\x l r -> if p x then l . (x:) . r else l . r) id t []

--differenza con il nodo successivo diff2Next

diff2next :: (Num a, Ord a, Show a, Read a) => BST a -> BST (a, Maybe a)
diff2next t = fst (fold f (\s -> (Void, s)) t Nothing)
  where
    f x l r s = let (rt, succ_x) = r s
                    (lt, first)  = l (Just x)
                in (Node (x, fmap (subtract x) succ_x) lt rt, first)


--limitedVisit

limitedVisit :: (Ord a, Show a, Read a) => a -> a -> BST a -> [a]
limitedVisit x y t = fold f id t []
  where
    f v l r acc = let rs = if v < y            then r acc else acc
                      ms = if x <= v && v <= y then v : rs else rs
                  in  if x < v                 then l ms  else ms

--shiftToZero

shiftToZero :: (Num a, Ord a, Show a, Read a) => BST a -> BST a
shiftToZero t = case fold f (Nothing, const Void) t of
                  (Nothing, _) -> Void
                  (Just m,  g) -> g m
  where
    f x (ml, l) (_, r) = ( Just (fromMaybe x ml)
                         , \m -> Node (x - m) (l m) (r m) )


