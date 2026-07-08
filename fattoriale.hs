--fattoriale

fatt :: (Integral a) => a -> a
fatt 0 = 1
fatt n = n * fatt (n-1)
