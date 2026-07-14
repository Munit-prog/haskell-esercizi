--elementi in posizione dispari
pos_dispari :: [a] -> [a]
pos_dispari [] = []
pos_dispari [x] = [x]
pos_dispari (x : y : resto) = x : pos_dispari resto

--somma degli elementi in posizione dispari
somma_dispari ::  Num a => [a] -> a
somma_dispari [] = 0
somma_dispari [x] = x
somma_dispari (x:y:resto) = x + somma_dispari resto

--QuickSort polimorfo
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort [x] = [x]
quicksort (x:resto) = quicksort(minori) ++ [x] ++ quicksort(maggiori)
  where minori = filter (<=x) resto
        maggiori = filter (>x) resto

--i due minori elementi dispari
minOdd :: (Ord a, Integral a) => [a] -> Maybe (a,a)
minOdd xs = case quicksort (filter odd xs) of
  x : y : _ -> Just (x,y)
  _ -> Nothing
