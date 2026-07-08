-- date due liste, costruisce  la lista di tutte le possibili coppie:
-- elemento della prima lista, elemento della seconda.
listacoppie xs ys = [(x,y) | x <- xs, y <- ys]

listacoppie2 [] ys = []
listacoppie2 (x:xs) ys = (listacoppieAux x ys) ++ (listacoppie2 xs ys)
  where
    listacoppieAux x [] = []
    listacoppieAux x (y:ys) = (x,y) : (listacoppieAux x ys)

listacoppie3 [] ys = []
listacoppie3 (x:xs) ys = (listacoppieAux x ys) ++ (listacoppie2 xs ys)
  where
    listacoppieAux x ys = map (\ y -> (x,y)) ys 

listacoppie4 xs ys = xs >>=  (\x -> ys >>= (\y -> [(x,y)]))


l1 = [1,2,3]
l2 = listacoppie4 l1 l1


-- Data una lista, costruire la lista, di liste, contenente
-- tutte le possibili permutazioni della lista originaria.

permutazioni [] = [[]]
permutazioni (x:xs) = inserisciList x (permutazioni xs)
  where
--  inserisciList x [] = []
--  inserisciList x (y : ys) = (inserisci x y) ++ (inserisciList x ys) 
    inserisciList x ys = ys >>= (\ y -> inserisci x y)
      where
         inserisci x [] = [[x]]
         inserisci x (y : ys) = (x : y : ys) : ( map (y:) (inserisci x ys)) 

m1 = permutazioni l1


-- Data una matrice, ben fatta, costruire la matrice trasposta.
trasposta ([]:_) = []
trasposta m = (map head m) : (trasposta (map tail m)) 

m2 = trasposta m1

data Tree a =  Tree a [Tree a]

tr1 = Tree 1 []
tr2 = Tree 1 [Tree 2 [], Tree 3 [Tree 4 []]]

-- Dato un albero, definire la lista che rappresenta il suo cammino massimo.
camminoMax (Tree x xt) =
  let (n, l) = selectMax (map camminoMax xt) 
      in (n + 1, x:l)
         where
           takeMax (n1, l1)(n2, l2) = if n1 < n2 then (n2, l2) else (n1, l1)
           selectMax l = foldl takeMax (-1,[]) l

c1 = camminoMax tr2

-- Dato un albero, calcolare il suo diametro.
altezza_diametro (Tree x tx) = evaluate (map altezza_diametro tx)
  where 
    updateMax (n1, n2) x  = if x <= n2 then (n1, n2)
                            else if x > n1 then (x, n1)
                                 else (n1, x)
                                      
    evaluate xs = let (m1,m2) = foldl updateMax (-1,-1) (map fst xs)
                      n       = foldl max       (-1)     (map snd xs)
                  in
                    (m1 + 1, max (m1 + m2 + 2) n)

r1 = altezza_diametro tr1
r2 = altezza_diametro tr2
