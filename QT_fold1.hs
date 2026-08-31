
data (Eq a, Show a) => QT a = C a | Q (QT a) (QT a) (QT a) (QT a)
 deriving (Eq, Show)

--definizione
fold :: (Eq a, Show a) => (b -> b -> b -> b -> b) -> (a -> b) -> QT a -> b
fold f g (C x) = g x
fold f g (Q q1 q2 q3 q4) = f (fold f g q1) (fold f g q2) fold f g q3) (fold f g q4)

--height 
height :: (Eq a, Show a) => QT a -> Int
height q = fold (\a b c d -> 1 + max (max a b) (max c d)) (const 0)


--length

length :: (Eq a, Show a) => QT a -> Int
length = fold (\a b c d -> 1 + a + b + c + d) (const 1)

--simply

simplify :: (Eq a, Show a) => QT a -> QT a
simplify = fold buildNSimplify C

--map
mapQT :: (Eq a, Show a, Eq b, Show b) => (a -> b) -> QT a -> QT b
mapQT h = fold buildNSimplify (C . h)

--flip

flipHorizontal :: (Eq a, Show a) => QT a -> QT a
flipHorizontal = fold (\t1 t2 t3 t4 -> Q t3 t4 t1 t2) C

flipVertical :: (Eq a, Show a) => QT a -> QT a
flipVertical = fold (\t1 t2 t3 t4 -> Q t2 t1 t4 t3) C

--rotazioni

rotate90Right :: (Eq a, Show a) => QT a -> QT a
rotate90Right = fold (\t1 t2 t3 t4 -> Q t3 t1 t4 t2) C

rotate90Left :: (Eq a, Show a) => QT a -> QT a
rotate90Left = fold (\t1 t2 t3 t4 -> Q t2 t4 t1 t3) C

rotate180 :: (Eq a, Show a) => QT a -> QT a
rotate180 = fold (\t1 t2 t3 t4 -> Q t4 t3 t2 t1) C

--simmetrica
isHorizontalSymmetric :: (Eq a, Show a) => QT a -> Bool
isHorizontalSymmetric t = flipHorizontal t == t

isVerticalSymmetric :: (Eq a, Show a) => QT a -> Bool
isVerticalSymmetric t = flipVertical t == t

isCenterSymmetric :: (Eq a, Show a) => QT a -> Bool
isCenterSymmetric t = rotate180 t == t

--rotazioni

rotations :: (Eq a, Show a) => QT a -> [QT a]
rotations t = [t, rotate90Right t, rotate180 t, rotate90Lfet t]

elem_or_mele :: (Eq a, Show a) => QT a -> [QT a] -> Bool
elem_or_mele t ts = any (`elem` ts) (rotations t)

--quanti pixel

howManyPixels :: (Eq a, Show a) => QT a -> Int
howManyPixels = fold (\n1 n2 n3 n4 -> 4 * max (max n1 n2) (max n3 n4)) (const 1)


--rifare occurencies con il fold

occurrencies :: (Eq a, Show a) => QT a -> a -> Int
occurrencies q c = fold nodo foglia q (height q)
  where foglia x k | x == c    = 4 ^ k
                   | otherwise = 0
        nodo f1 f2 f3 f4 k = f1 (k-1) + f2 (k-1) + f3 (k-1) + f4 (k-1)


--nuova zipWith


zipWithQT :: (Eq a, Show a, Eq b, Show b, Eq c, Show c)
          => (a -> b -> c) -> QT a -> QT b -> QT c
zipWithQT op = fold nodo foglia
  where foglia x q2 = mapQT (op x) q2
        nodo f1 f2 f3 f4 q2 = buildNSimplify (f1 u1) (f2 u2) (f3 u3) (f4 u4)
          where (u1, u2, u3, u4) = subs q2


--insertpict


insertPict :: (Eq a, Show a) => QT a -> QT a -> QT Bool -> QT a
insertPict qt qf m = fold nodo foglia m qt qf
  where foglia b t f = if b then t else f
        nodo f1 f2 f3 f4 t f = buildNSimplify (f1 t1 u1) (f2 t2 u2) (f3 t3 u3) (f4 t4 u4)
          where (t1,t2,t3,t4) = subs t
                (u1,u2,u3,u4) = subs f

--inserire logo
insertLogo :: (Eq a, Show a) => QT a -> QT a -> QT Bool -> QT a
insertLogo ql qp m =
    buildNSimplify p1 p2 p3
      (buildNSimplify r1 r2 r3
         (buildNSimplify (insertPict ql s1 m) s2 s3 s4))
  where (p1,p2,p3,p4) = subs qp     -- primo livello:  si scende in p4
        (r1,r2,r3,r4) = subs p4     -- secondo livello: si scende in r4
        (s1,s2,s3,s4) = subs r4     -- terzo livello:  si agisce su s1


--maschera


commonPoints :: (Eq a, Show a) => [QT a] -> QT Bool
commonPoints (q:qs) = go qs
  where go []     = C True
        go (p:ps) = zipWithQT (&&) (zipWithQT (==) q p) (go ps)

--predicato booleano sul contorno

framed :: (Eq a, Show a) => (a -> Bool) -> QT a -> Bool
framed p q = fold nodo foglia q (True, True, True, True)
  where foglia x (t,b,l,r) = not (t || b || l || r) || p x
        nodo f1 f2 f3 f4 (t,b,l,r) =
             f1 (t, False, l, False)
          && f2 (t, False, False, r)
          && f3 (False, b, l, False)
          && f4 (False, b, False, r)

--frame 
angoloNW :: (Eq a, Show a) => QT a -> a
angoloNW (C x)        = x
angoloNW (Q t1 _ _ _) = angoloNW t1

frame :: (Eq a, Show a) => QT a -> Maybe a
frame q | framed (== c) q = Just c
        | otherwise       = Nothing
  where c = angoloNW q
