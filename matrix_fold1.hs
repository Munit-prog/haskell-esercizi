
--definizione
type Matrix a = [[a]]

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z [] = z
foldr f z (x:xs) = f x (foldr f z xs)

--colsums

addVec :: Num a => [a] -> [a] -> [a]
addVec [] ys = ys
addVec xs [] = xs
addVec (x:xs) (y:ys) = (x + y) : addVec xs ys

colSums :: Num a => [[a]] -> [a]
colSums m = foldr addVec [] m


--colaltsums

subVec :: Num a => [a] -> [a] -> [a]
subVec [] ys = map negate ys
subVec xs [] = xs
subVec (x:xs) (y:ys) = (x - y) : subVec xs ys

colaltsums :: Num a => [[a]] -> [a]
colaltsums m = foldr subVec [] m


--colMinMax

minMaxVec :: Ord a => [a] -> [(a,a)] -> [(a,a)]
minMaxVec [] ps = ps
minMaxVec xs [] = map (\x -> (x,x)) xs
minMaxVec (x:xs) ((mn,mx) : ps) = (min x mn, max x mx) : minMaxVec xs ps


colMinMax :: Ord a => [[a]] -> [(a,a)}
colMinMax m = foldr minMaxVec [] m


--triangolare inferiore?

lowertriangular :: (Num a, Eq a) => [[a]] -> Bool
lowertriangular m = fst (foldl step (True, 1) m)
 where step (ok, i) r = (ok && all (== 0) (drop i r), i+1)

--triangolare superiore?

uppertriangular :: (Num a, Eq a) => [[a]] -> Bool
uppertriangular m = fst (foldl step (True, 0) m)
 where step (ok, k) r = (ok && all (== 0) (take k r), k + 1)


--diagonale 

diagonal :: (Num a, Eq a) => [[a]] -> Bool
diagonal m = fst (foldl step (True, 0) m)
 where step (ok, k) r = (ok && all (== 0) (take k r) && all (== 0) (drop (k + 1) r), k + 1)

soloDiagonale :: (Num a, Eq a) => Int -> [a] -> Bool
soloDiagonale _ [] = True
soloDiagonale 0 (_:xs) = all (== 0) xs
soloDiagonale k (x:xs) = x == 0 && soloDiagonale (k-1) xs


--convergent

convergent :: (Num a, Ord a) => [[a]] -> a -> Bool
convergent m r = fst (foldl step (True, 0) m)
 where step (ok, k) row = (ok && abs (sum (take k row) + sum (drop (k+1) row)) < r, k + 1)


sommaFuoriDiagonale :: Num a => Int -> [a] -> a
sommaFuoriDiagonale _ [] = 0
sommaFuoriDiagonale 0 (_:xs) = sum xs
sommaFuoriDiagonale k (x:xs) = x + sommaFuoriDiagonale (k-1) xs


--trasposta

inTesta :: [a] -> [[a]] -> [[a]]
inTesta [] css = css
inTesta xs [] = map (\x -> [x]) xs
inTesta (x:xs) (cs:css) = (x:cs) : inTesta xs css

trasposta :: [[a]] -> [[a]]
trasposta m = foldr inTesta [] m

--simmetrica

isSymmetric :: Eq a => [[a]] -> Bool
isSymmetric m = m == trasposta m


--matrice prodotto date due matrici nxk e kxm

prodottoScalare :: Num a => [a] -> [a] -> a
prodottoScalare xs ys = foldr (+) 0 (zipWith (*) xs ys

matMul :: Num a => [[a]] -> [[a]] -> [[a]]
matMul a b = map (\riga -> map (prodottoScalare riga) bt) a
 where bt = trasposta b
