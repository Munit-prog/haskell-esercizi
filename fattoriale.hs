--fattoriale

fatt :: (Integral a) => a -> a
fatt 0 = 1
fatt n = n * fatt (n-1)

--combinazioni

comb :: (Integral a) => a -> a -> a
comb n k = fatt n `div` (fatt k * fatt (n-k))
