{- Su liste aventi come elementi coppie di valori di uno stesso tipo
ordinabile, scrivere le seguenti funzioni Haskell:

- una funzione che data una lista restituisce la lista contenente le
sole coppie ordinate (il primo elemento della coppia â€˜e minore del
secondo);
- una funzione che data una lista di coppie le ordina, ossia scambia
tra loro gli elementi di una coppie in modo che il primo sia minore
del secondo;
- considerando lâ€™ordine lessicografico tra le coppie, definire una
funzione che data una lista di coppie la ordina.

Giocando sul fatto di usare o meno: le funzioni â€™foldâ€˜, la ricorsione di
coda o le funzioni di libreria, fornire unâ€™implentazione alternativa per
ciascuna delle funzioni precedenti. -}

slo [] = []
slo ((x1,x2) : xs) | (x1 <= x2)  = (x1,x2) : slo(xs)
                   | otherwise   = slo xs

slo2 xs = filter (\ (x1, x2) -> x1 <= x2) xs

slo3 :: Ord(a) => [(a,a)] -> [(a,a)]
slo3 xs  = filter (uncurry (<=)) xs

slo4 [] ys = ys
slo4 ((x1,x2) : xs) ys | (x1 <= x2)  = slo4 xs ((x1,x2) : ys)
                       | otherwise   = slo4 xs ys


olc [] = []
olc ((x1,x2) : xs) | (x1 <= x2)  = (x1,x2) : (olc xs)
                   | otherwise   = (x2,x1) : (olc xs)


olc2 xs = map ( \ (x1, x2) -> if x1 <= x2 then (x1,x2) else (x2,x1)) xs

{- dichiarazione non necessaria, presente in libreria  
instance (Ord a, Ord b) => Ord (a,b) where
    (<) (x1,y1)(x2,y2) = (x1 < x2) || (x1 == x2) && (y1 <  y2)  
    (<=)(x1,y1)(x2,y2) = (x1 < x2) || (x1 == x2) && (y1 <= y2)
-}


{- Scrivere le seguenti funzioni Haskell:

- una funzione che data una lista, conta per quante volte il primo
carattere si ripete consecutivamente nella lista, o in altre parole,
qualâ€™Ã¨ la lunghezza della massima lista iniziale contente caratteri
tutti uguali.
- una funzione che data una lista, determina la lunghezza della piÃ¹
lunga seguenza di caratteri consecutivi uguali allâ€™interno della lista.

Giocando sul fatto di usare o meno: le funzioni fold, la ricorsione di
coda o le funzione precedentemente definite, fornire unâ€™implentazione
alternativa per ciascuna delle funzioni precedenti.
-}

rpt [] = 0
rpt [x] = 1
rpt (x1: x2 : xs) | (x1 == x2) = rpt (x2: xs) + 1
                  | otherwise  = 1

rptRC [] acc = acc
rptRC [x] acc = acc + 1
rptRC (x1: x2 : xs) acc | (x1 == x2) = rptRC (x2: xs) (acc + 1)
                        | otherwise = (acc + 1)

rpt2 [] = 0
rpt2 (x:xs) = rptAux x xs
  where
    rptAux x [] = 1
    rptAux x (x1:xs) | (x == x1)  = (rptAux x xs) + 1
                 | otherwise  = 1

rpt3 xs = foldr (\ x n -> if x == (head xs)  then n+1 else 0) 0 xs  

rptL [] = (0, [])
rptL [x] = (1, [])
rptL (x1: x2 : xs) | (x1 == x2) = incFst (rptL (x2: xs))
                   | otherwise  = (1, (x2:xs))
  where
    incFst (x,y) = (x+1,y)

rptC []= 0
rptC xs = let (n,ys) = rptL xs in max n (rptC ys)
