

--definizione

data (Eq a,Show a) => Tree a = Void | Node a [Tree a]
 deriving (Eq, Show)

data (Eq a, Show a) => NonEmptyTree a = Node [NonEmptyTree a]
 deriving (Eq, Show)

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z [] = z
foldr f z (x:xs) = f x (foldr f z xs)

--treefold

treefold :: (Eq a, Show a) => (a -> [b] -> b) -> b -> Tree a -> b
treefold f z Void = z
foldr f z (x:xs) = f x (foldr f z xs)

--height

height :: (Eq a, Show a) => Tree a -> Int
height = treefold (\_ hs -> 1 + maximum (-1 : hs)) (-1)

--simplify

simplify :: (Eq a, Show a) => Tree a -> Tree a
simplify = treefold (\x ts -> Node x (filter (/= Void) ts)) Void

--fold senza funzione di aggregazione

treefoldr :: (Eq a, Show a) => (a -> b -> c) -> c -> (c -> b -> b) -> b -> Tree a -> c
treefoldr f z g z' Void = z
treefoldr f z g z' (Node x ts) = f x (aggr ts)
 where aggr [] = acc
       aggr (t:ts') = g (treefoldr f z g z' t) (aggr ts')

treefoldl :: (Eq a, Show a) => (b -> a -> c) -> c -> (c -> b -> b) -> b -> Tree a -> c
treefoldl f z g z' Void = z
treefoldl f z g z' (Node x ts) = f (aggr z' ts) x
 where aggr acc [] = acc

--nuovo height 
height :: (Eq a, Show a) => Tree a -> Int
height = treefoldr (\_ m -> 1 + m) (-1) max (-1)

--simplify

simplify :: (Eq a, Show a) => Tree a -> Tree a
simplify = treefoldr Node Void cons []
 where cons Void ts = ts
       cons t    ts = t : ts

--degree

degree :: (Eq a, Show a) => Tree a -> Int
degree = treefold (\_ ds -> maximum (length (filter (>= 0) ds) : ds)) (-1)

--transpose

transpose :: (Eq a, Show a) => Tree a -> Tree a
transpose = treefold (\x ts -> Node x (reverse ts)) Void

--issym
shape :: (Eq a. Show a) => Tree a -> Tree ()
shape = treefold (\_ ts -> Node () ts) Void

issymm :: (Eq a, Show a) => Tree a -> Bool
issymm t = s == transpose s
 where s = shape (simplify t)

--normalize
normalize :: (Integral a, Show a) => Tree a -> Tree Double
normalize t = treefold (\x ts -> Node (fromIntegral x * k) ts) Void t
 where k = 1 / fromIntegral (height t)

--annotate

annotate :: (Eq a, Show a) => Tree a -> Tree (a, Int)
annotate = treefold f Void 
 where f x ts = Node (x, 1 + maximum (-1 : map h0f ts)) ts
  h0f Void = -1
  h0f (Node (_, h) _) = h

--isCorrect
iscorrect :: (Eq a, Show a) => (a -> [[a]]) -> Tree a -> Bool
iscorrect g = snd . treefold f ([], True)
 where f x ps = ([x], ok && all snd ps)
  where syms = concat (map fst ps)
     ok | null (g x) = null syms
        | otherwise = syms `elem` g x

--diameter

diameter :: (Eq a, Show a) => Tree a -> Int
diameter = snd . treefold f (-1, -1)
 where f _ ps = (h, maximum (h + s : map snd ps))
  where (h, s) = top2 0 0 (map (\(hc,_) -> hc + 1) ps)
   top2 b s []       = (b, s)
   top2 b s (c:cs) | c > b = top2 c b cs
                   | c > s = top2 b c cs
                   | otherwise = top2 b s cs

--maxpathweight
maxPathWeight :: (Show a, Ord a, Num a) => Tree a -> a
maxPathWeight = snd . treefold f (0, 0)
 where f x ps = (down, maximum (through : map snd ps)
   where (d1, d2) = top2 0 0 (map fst ps)
    down = x + d1
    through = x + d1 + d2
  top2 b s [] = (b, s)
  top2 b s (c:cs) | c > b = top2 c b cs
                  | c > s = top2 b c cs
                  | otherwise = top2 b s cs

