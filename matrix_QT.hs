--definizione

data (Eq a, Num a, Show a) => Mat a {
 nexp :: Int,
 mat :: QT a
 }
 deriving (Eq, Show)

--determinare se è triangolo inferiore

lowertriangular :: (Eq a, Num a, Show a) => Mat a -> Bool
lowertriangular (Mat n q) = ltQT n q

ltQT :: (Eq a, Num a, Show a) => Int -> QT a -> Bool
ltQT 0 (C _) = True
ltQT _ (C x) = x == 0
ltQT n (Q nw ne _ se) = ne == C 0 && ltQT (n-1) nw && ltQT (n-1) se

--determinare se è trinagolare superiore

uppertriangular :: (Eq a, Num a, Show a) => Mat a -> Bool
uppertriangular (Mat n q) = utQT n q

utQT :: (Eq a, Num a, Show a) => Int -> QT a -> Bool
utQT 0 (C _) = True
utQT _ (C x) = x == 0
utQT n (Q nw _ sw se) = sw = C 0 && utQT (n-1) nw && utQT (n-1) se

--è diagonale?

diagonal :: (Eq a, Num a, Show a) => Mat a -> Bool
diagonal (Mat n q) = dgQT n q

dgQT :: (Eq a, Num a, Show a) => Int -> QT a -> Bool
dgQT 0 (C _) = True
dgQT n (Q nw ne sw se) = ne == C 0 && sw == C 0 && dgQT (n-1) nw && dgQT (n-1) se


--date due matrici calcoalre la somma

matSum :: (Eq a, Num a, Show a) => Mat a -> Mat a -> Mat a
matSum (Mat n1 q1) (Mat n2 q2)
 | n1 /= n2 = error "matrici dimensioni diverse"
 | otherwise = Mat n1 (qtSum q1 q2)

qtSum :: (Eq a, Num a, Show a) => QT a -> QT a -> QT a
qtSum (C x) (C y) = C (x + y)
qtSum (C x) (Q b1 b2 b3 b4) = mkQ (qtSum (C x) b1) (qtSum (C x) b2) (qtSum (C x) b3) (qtSum (C x) b4)
qtSum (Q a1 a2 a3 a4) (C y) = mkQ (qtSum a1 (C y)) (qtSum a2 (C y)) (qtSum a3 (C y)) (qtSum a4 (C y))

mkQ :: (Eq a, Num a, Show a) => QT a -> QT a -> QT a -> QT a -> QT a
mkQ (C x1) (C x2) (C x3) (C x4)
 | x1 == x2 && x2 == x3 && x4 = C x1
mkQ q1 q2 q3 q4 = Q q1 q2 q3 q4

--date due matrici calcolare il prodotto

matMul :: (Eq a, Num a, Show a) => Mat a -> Mat a -> Mat a
matMul (Mat n1 q1) (Mat n2 q2)
 | n1 /= n2 = error "matrici dimensioni diverse"
 | otherwise = Mat n1 (qtMul n1 q1 q2)

qtMul :: (Eq a, Num a, Show a) => Int -> QT a -> QT a -> QT a
qtMul n (C x) (C y) = C (fromIntegral (2^n) * x * y)
qtMul n q1 q2 = mkQ (qtSum (mul a1 b1) (mul a2 b3)) (qtSum (mul a1 b2) (mul a2 b4)) (qtSum (mul a3 b1) (mul a4 b3)) (qtSum (mul a3 b2) (mul a4 b4))
 where (a1, a2, a3, a4) = subs q1
       (b1, b2, b3, b4) = subs q2
       mul              = qtMul (n-1)

subs :: (Eq a, Num a, Show a) => QT a -> (QT a, QT a, QT a, QT a)
subs (C x)       = (C x,C x,C x,C x)
subs (Q q1 q2 q3 q4) = (q1, q2, q3, q4)


--zong

zong :: (Eq a, Num a, Show a) => a -> a -> Mat a -> Mat a
zong x y (Mat n q) = Mat n (qtZong n x y q)

qtZong :: (Eq a, Num a, Show a) => Int -> a -> a -> QT a -> QT a
qtZong 0 x y (C v) = C (x * v - y) 
qtZong n x y 1     = mkQ (qtZone (n-1) x y q1) (scalQT x q2) (scalQT x q3) (qtZong (n-1) x y q4)
 where (q1, q2, q3, q4) = subs q

scalQT :: (Eq a, Num a, Show a) => a -> QT a -> QT a
scalQT x (C v)           = C (x * v)
scalQT x (Q q1 q2 q3 q4) = mkQ (scalQT x q1) (scalQT x q2) (scalQT x q3) (scalQT x q4)


--che calcola lo scalare
data (Eq a, Num a, Show a) => BT a = L a | B (BT a) (BT a)
 deriving (Eq, Show)

data (Eq a, Num a, Show a) => Vec a = Vec {
 vexp :: Int,
 vec :: BT a }
 deriving (Eq, Show) 

f :: (Eq a, Num a, Show a) => Vec a -> Mat a -> a
f (Vec m v) (Mat n q)
 | m /= n = error "vettore e matrice incompatibili"
 |otherwise = vqv n v q v

vqv :: (Eq a, Num a, Show a) => Int -> BT a -> QT a -> BT a -> a
vqv n (L u) (C c) (L w) = fromIntegral (4^n) * u * c* w
vqv n bu q bw = vqv (n-1) u1 a1 w1 + vqv (n-1) u1 a2 w2 + vqv (n -1) u2 a3 w1 + vqv (n-1) u2 a4 w2
 where (u1, u2) = halves bu
       (w1, w2) = halves bw
       (a1, a2, a3, a4) = subs q

halves :: (Eq a, Num a, Show a) => BT a -> (BT a, BT a)
halves (L x)     = (L x, L x)
halves (B l r)   = (l, r)

--colSums

colSums :: (Eq a, Num a, Show a) => Mat a -> [a]
colSums (Mat n q) = csQT n q

csQT :: (Eq a, Num a, Show a) => Int -> QT a -> [a]
csQT n (C x) = replicate (2^n) (fromIntegral (2^n) * x)
csQT n (Q q1 q2 q3 q4) = vsum (csQT (n-1) q1) (csQT (n-1) q3) ++ vsum (csQT (n-1) q2) (csQT (n-1) q4)

vsum :: Num a => [a] -> [a] -> [a]
vsum [] []           = []
vsum (x:xs) (y:ys)   = (x + y) : vsum xs ys


--rowSums

rowSums :: (Eq a, Num a, Show a) => Mat a -> [a]
rowSums (Mat n q) = rsQT n q

rsQT :: (Eq a, Num a, Show a) => Int -> QT a -> [a]
rsQT n (C x) = replicate (2 ^ n) (fromIntegral (2^n) * x)
rsQT n (Q q1 q2 q3 q4) = vsum (rsQT (n-1) q1) (rsQT (n-1) q2) ++ vsum (rsQT (n-1) q3) (rsQT (n-1) q4)


--colMinMax
colMinMax :: (Ord a, Eq a, Num a, Show a) => Mat a -> [(a,a)]
colMinMax (Mat n q) = cmmQT n q

cmmQT :: (Ord a, Eq a, Num a, Show a) => Int -> QT a -> {(a,a)}
cmmQT n (C x) = replicate (2^n) (x,x)
cmmQT n (Q q1 q2 q3 q4) = vmm (cmmQT (n-1) q1) (cmmQT (n-1) q3) ++  vmm (cmmQT (n-1) q2) (cmmQT (n-1) q4)

vmm :: Ord a => [(a,a)] -> [(a,a)] -> [(a,a)}
vmm [] [] = []
vmm ((m1, x1): ps) ((m2, x2):qs) = (min m1 m2, max x1 x2) : vmm ps qs


--colVar calcolo variazioni delle matrice (massimo - minimo)

colVar :: (Ord a, Eq a, Num a, Show a) => Mat a -> [a]
colVar m = map (\(mn, mx) -> mx - mn) (colMinMax m)


--colAltSums

colAltSums :: (Eq a, Num a, Show a) => Mat a -> {a]
colAltSums (Mat n q) = casQT n q

casQT :: (Eq a, Num a, Show a) => Int -> QT a -> [a]
casQT 0 (C x) = [x]
castQT n (Q q1 q2 q3 q4)
 | n == 1 = vdiff (casQT 0 q1) (casQT 0 q3) ++ vdiff (casQT 0 q2) (casQT 0 q4)
 | otherwise = vsum (casQT (n-1) q1) (casQT (n-1) q3) ++ vsum (casQT (n-1) q2) (casQT (n-1) q4)

vdiff :: Num a => [a] -> [a] -> [a]
vdiff [] [] = []
vdiff (x:xs) (y:ys) = (x - y) : vdiff xs ys
