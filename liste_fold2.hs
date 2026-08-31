--definizione

foldr :: (a -> c -> c) -> c -> [a] -> c

--coppie pair
pairsPrefix :: Num a => [a] -> [(a, a)]
pairsPrefix l = foldr f b l 0
  where b     = \_ -> []
        f x r = \a -> (x, a) : r (a + x)

--elementi diminuiti

shiftToZero :: (Ord a, Num a) => [a] -> [a]
shiftToZero []      = []
shiftToZero l@(x:_) = g m
  where (m, g)   = foldr f (x, const []) l
        f y (n, h) = (min y n, \k -> (y - k) : h k)

