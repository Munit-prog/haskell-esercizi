--calcolo dimensioni matrice se ben formata
matrix_dim :: [[a]] -> (Int, Int)
matrix_dim [] = (0,0)
matrix_dim m
  | all (==l) ls = (length m, l)
  | otherwise = (-1, -1)
  where ls = map length m
        l  = head ls

