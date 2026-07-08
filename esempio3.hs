insert2 :: a -> [a] -> [[a]]

insert2 x [] = [[x]]
insert2 x (y : ys) =  (x : y : ys) : map (y:) (insert2 x ys)


perms :: [a] -> [[a]]
perms [] = [[]]
perms (x : xs) = do ys <- perms xs
                    insert2 x ys  
  



coppie1 xs ys = [(x,y) | x <- xs, y <- ys]

coppie2 xs ys = do x <- xs
                   y <- ys
                   return (x,y)

coppie3 [] ys = []
coppie3 (x:xs) ys = map (\y -> (x,y)) ys ++ coppie3 xs ys

remove1 x [] = Nothing
remove1 x (y : ys) | (x == y)  = Just ys
                   | otherwise = do ys1 <- remove1 x ys 
                                    Just (y:ys1)


remove2 x [] = Nothing
remove2 x (y : ys) | (x == y)  = Just ys
                   | otherwise = case (remove2 x ys) of
                                   Nothing -> Nothing
                                   Just ys1 -> Just (y : ys1)

compare1 [] [] = True
compare1 [] _  = False
compare1 (x:xs) ys =  case (remove1 x ys) of
                        Nothing -> False
                        Just ys1 -> compare1 xs ys1

compare2 xs ys = foldr (\ x zs -> case zs of
                                    Nothing -> Nothing
                                    Just zs1 -> (remove2 x zs1)) (Just ys) xs


perms [] = [[]]
perms (x:xs) = do ys <- perms xs
                  insert1 x ys
                  
insert1 x [] = [[x]]
insert1 x (y:ys) =  (x:y:ys) : map (y:) (insert1 x ys)

length1 xs = foldr (\_ -> (+1))0 xs

matrice ln xxs = foldr (\ xs b -> b && (length1 xs == ln)) True xxs

quadrata xxs = foldr (\ xs b -> b && (length1 xs == length1 xxs)) True xxs

quadrata2 [] = True
quadrata2 (xs:xxs) = length1 xs == length1 (xs:xxs) && matrice2 (length1 xs) xxs

matrice2 n [] = True
matrice2 n (xs:xxs) = length1 xs == n && matrice2 n xxs

sublist [] _  = True
sublist _  [] = False
sublist (x:xs) (y:ys) | x == y    = sublist xs ys
                      | otherwise = False

palindroma xs = xs == reverse xs

lungPal [] = 0
lungPal xs = lungPalAux xs (reverse xs)

lungPalAux xs ys | sublist xs ys = length xs
                 | otherwise     = lungPalAux (tail xs) ys

lungPal2 [] = 0
lungPal2 (x : xs) = max (lungPal (reverse (x:xs))) (lungPal2 xs)



