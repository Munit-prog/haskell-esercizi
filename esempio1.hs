{-# LANGUAGE FlexibleContexts #-}
----------------------------------------------
-- alcune semplici dichiarazioni di vario tipo
----------------------------------------------

primes = filterPrime [2..] 
   where filterPrime (p:xs) = 
             p : filterPrime [x | x <- xs, x `mod` p /= 0]
first_primes = take 100 primes

n1 = 5 + 3  :: Num a => a
n2 = pi + 3.0
n3 = 3 + pi    --- accettato, a 3 associato un tipo numerico generico
--  n4 =   	(rem 3 4) + pi   --- genera errore di tipo:

enupla1 =  ('b', 5, pi) :: (Char, Integer, Double)
enupla2 =  ('b', ((5, pi), (1,2,3))) --- naturalmente posso iterare

list1 = [1,2,4] -- :: [Integer]
-- list2 = [1,2,'a']  -- type error
list3 = ['a','b','d'] -- == "abd"

x = y + 3
y = 0

--------------------
-- dichiarazioni di tipo
--------------------

data Colore = Rosso | Verde | Marrone

data Tree a  = Leaf a | Branch (Tree a) (Tree a)
-- data BTree a b  = LLeaf a | BBranch b (BTree a b) (BTree a b)
data List a   = Nil   | Cons  a (List a)


-- type String = [Char]  --- predefinito
type Person = (Name,Address)
type Name  = String 
data Address = None | Addr String

type IntTree = Tree Integer
type BinaryFunction a = a -> a -> a
type BinaryIntFunction = BinaryFunction Integer 


------------------
-- dichiarazione di funzioni
------------------

inc x = x + 1
inc2 = \ x -> x + 1 --- definizione equivalente

add x y = x + y
inc3 = add 1

add2  = \ x -> ( \ y -> (x + y ))
add3  = \ x y -> x + y

fact 0 = 1
fact n = n * fact (n -1)

fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fib2 0 = (0,1)
fib2 n = (snd pair, fst pair  + snd pair)
  where pair = fib2(n-1)

uncurryAdd (x, y) = x + y
uncurryAdd   :: Num a => (a, a) -> a 
n12 = uncurryAdd(5,7)

cUrry        :: ((a, b) -> c) -> a -> b -> c
cUrry f x y  =  f (x, y)
cUrry2 f =  \ x y ->  f (x, y)

uNcurry          :: (a -> b -> c) -> (a, b) -> c
uNcurry f (x, y) =  f x y
uNcurry2 f = \ (x, y) -> f x y

compose :: (b -> c) -> (a -> b) -> a -> c
compose f g x = f ( g x )


add3Values x y z = x + y + z
sei = (1 `add3Values` 2) 3

----------------
-- ogggetti infiniti
-----------------

addList (x:xs)(y:ys) = (x + y) : addList xs ys

fibList = 1:(addList fibList (0 : fibList))


reverse1 [] = []
reverse1 (x:xs) = reverse1(xs) ++ [x]

reverseAcc [] ys = ys
reverseAcc (x:xs) ys = reverseAcc xs (x:ys)
reverse2 xs = reverseAcc xs []

reverseFoldL xs  = foldl (\ xs -> (:xs)) [] xs
reverseFoldR xs = foldr (\ x -> (++ [x])) [] xs

sign x = case x  of
           y | y > 0  -> 1
             | y == 0 -> 0
             | y < 0  -> -1


listaCoppie [] ys = []
listaCoppie (x:xs) ys = (listaCoppieAux x ys) ++ listaCoppie xs ys
  where
    listaCoppieAux x [] = []
    listaCoppieAux x (y: ys) = (x,y) : listaCoppieAux x ys

listaCoppie2 [] ys     = []
listaCoppie2 (x:xs) ys = (map (\ y  -> (x,y)) ys ) ++ listaCoppie2 xs ys

listaCoppie3 xs ys = [ (x,y) | x <- xs, y <- ys ]


listaCoppie4 xs ys =
  case xs  of
    []     -> []
    (x:xs) -> (map (\ y  -> (x,y)) ys ) ++ listaCoppie4 xs ys

 

