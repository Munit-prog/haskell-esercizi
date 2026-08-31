
--definizione
data (Eq a, Num a, Show a) => Mat a = Mat {
 nexp :: Int,
 mat :: QT a
 }
 deriving (Eq, Show)

fold :: (a -> b) -> (b -> b -> b -> b -> b) -> QT a -> b

-- b=QT a

--transpose
transpose :: (Eq a, Num a, Show a) => Mat a -> Mat a
transpose (Mat n q) = Mat n (transposeQT q)

transposeQT :: (Eq a, Num a, Show a) => QT a -> QT a
transposeQT = fold C (\no ne so se -> Q no so ne se)

--isSymmetric

isSymmetric :: (Eq a, Num a, Show a) => Mat a -> Bool
isSymmetric (Mat _ q) = isTranspOf q q

isTranspOf :: (Eq a, Num a, Show a) => QT a -> QT a -> Bool
isTranspOf = fold (\v t -> t == C v)
                  (\h1 h2 h3 h4 t -> case t of
                                       C _           -> False
                                       Q t1 t2 t3 t4 -> h1 t1 && h2 t3
                                                             && h3 t2 && h4 t4)


