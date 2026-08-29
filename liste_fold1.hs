

--forma base fold

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z [] = z
foldr f z (x:xs) = f x (foldr f z xs)

---lista di coppie

sommeSuccessive :: Num a => [a] -> [(a,a)]
sommeSuccessive xs = fst (foldr f ([], 0) xs)
 where f x (ps, s) = ((x, s) : ps, x + s)

f :: a -> ([(a, a)], a)
z :: ([(a, a)], a)
