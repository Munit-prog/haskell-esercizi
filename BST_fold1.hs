
data (Ord a, Show a, Read a) => BST a = Void | Node {
 val :: a
 left, right :: BST a
 }
  deriving (Eq, Ord, Read, Show)

fold :: (Ord a) => (a -> b -> b -> b) -> b -> BST a -> b

--height

treeheight :: (Ord a) => BST a -> Integer
treeheight = fold (\_ hl hr -> 1 + max hl hr) 0

--annotate 
annotate :: (Ord a) => BST a -> BST (a, Int)
annotate = fold f Void
 where 
  f x sx dx = Node (x, 1 + max (rootH sx) (rootH dx)) sx dx
  rootH :: BST (b, Int) -> Int
  rootH Void = -1
  rootH (Node (_,h) _ _ ) = h


--almostBalanced

almostBalanced :: (Ord a) => BST a -> Bool
almostBalanced = fst . fold f (True, -1)
 where 
  f _ (bs, hs) (bd, hd) = (bs && bd && abs (hs - hd) <= 1, 1 + max hs hd)

--massimo dei diametri

diameter :: (Ord a) => BST a -> Int
diameter = snd . fold f (-1, -1)
 where
  f _ (hs, ds) (hd, dd) = (1 + max hs hd, max (max ds dd) (hs + hd +2))

maxDiameter :: (Ord a) => [BST a] -> Int
maxDiameter [] = -1
maxDiameter (t:ts) = max (diameter t) (maxDiameter ts)

--è un BST?

isBST :: (Ord a) => BST a -> Bool
isBST t = b
  where
    (b, _, _) = fold f (True, Nothing, Nothing) t

    f x (bs, mns, mxs) (bd, mnd, mxd) =
      ( bs && bd && maxLess mxs x && minGreater mnd x
      , minM (Just x) (minM mns mnd)
      , maxM (Just x) (maxM mxs mxd) )

    maxLess    Nothing  _ = True
    maxLess    (Just m) x = m < x

    minGreater Nothing  _ = True
    minGreater (Just m) x = x < m

    minM Nothing   m          = m
    minM m         Nothing    = m
    minM (Just p)  (Just q)   = Just (min p q)

    maxM Nothing   m          = m
    maxM m         Nothing    = m
    maxM (Just p)  (Just q)   = Just (max p q)

--isAvl
data (Ord a) => ABST a = Void | Node Bal a (ABST a) (ABST a)
 deriving (Eq, Ord, Read, Show)

data Bal = Left | Bal | Right deriving (Eq, Ord, Read, Show)


foldA :: (Ord a) => (Bal -> a -> b -> b -> b) -> b -> ABST a -> b
foldA _ z Void            = z
foldA f z (Node b x sx dx) = f b x (foldA f z sx) (foldA f z dx)

isAVL :: (Ord a) => ABST a -> Bool
isAVL = fst . foldA f (True, -1)
  where
    f b _ (bs, hs) (bd, hd) =
      ( bs && bd && consistente b (hd - hs)
      , 1 + max hs hd )

    consistente Left  (-1) = True
    consistente Bal     0  = True
    consistente Right   1  = True
    consistente _       _  = False
--RBT

data (Ord a) => RBT a = Void | Node a Color (RBT a) (RBT a)
 deriving (Eq, Ord, Read, Show)
data Color = Red | Black deriving (Eq, Ord, Read, Show)

foldR :: (Ord a) => (a -> Color -> b -> b -> b) -> b -> RBT a -> b
foldR _ z Void             = z
foldR f z (Node x c sx dx) = f x c (foldR f z sx) (foldR f z dx)


isRBT :: (Ord a) => RBT a -> Bool
isRBT t = radiceNera t && valido (foldR f (Info Nothing Nothing 0 Black) t)
  where
    radiceNera Void           = True
    radiceNera (Node _ c _ _) = c == Black

    valido Bad = False
    valido _   = True

    f _ _ Bad _ = Bad
    f _ _ _ Bad = Bad
    f x c (Info mns mxs bhs cs) (Info mnd mxd bhd cd)
      | not (maxLE mxs x)                    = Bad
      | not (minGT mnd x)                    = Bad
      | bhs /= bhd                           = Bad
      | c == Red && (cs == Red || cd == Red) = Bad
      | otherwise = Info (minM (Just x) (minM mns mnd))
                         (maxM (Just x) (maxM mxs mxd))
                         (bhs + nero c)
                         c

    nero Black = 1
    nero Red   = 0

    maxLE Nothing  _ = True
    maxLE (Just m) x = m <= x

    minGT Nothing  _ = True
    minGT (Just m) x = x < m

    minM Nothing  m         = m
    minM m        Nothing   = m
    minM (Just p) (Just q)  = Just (min p q)

    maxM Nothing  m         = m
    maxM m        Nothing   = m
    maxM (Just p) (Just q)  = Just (max p q)
