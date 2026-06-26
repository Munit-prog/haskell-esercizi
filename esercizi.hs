--raddoppio
doppio :: Int->Int
doppio x=x*2
--Int -> Int si legge "funzione che prende un Int e restituisce un int"
--somma
somma :: Int->Int->Int
somma x y =x+y
--fattoriale (caso base + passo ricorsivo)
fattoriale :: Integer -> Integer
fattoriale 0=1
fattoriale n = n * fattoriale (n-1)
-- Integer (interi a precisione arbitraria) invece di Int (dimensione fissa)
--Fibonacci
fib::Integer->Integer
fib 0 = 0
fib 1= 1
fib n = fib (n-1) + fib (n-2)
--ricorsione sulle liste elemento posto in testa al resto x:xs, dove : operatore cons)
sommaLista :: [Integer] -> Integer
sommaLista []  = 0
sommaLista (x:xs) = x + sommaLista xs
--si legge: somma vuota=0, somma di una lista con testa x e coda xs è x più la somma di xs, fa
--x + ricorsione xs, quindi si somma x più il resto
--lunghezza lista
lunghezza :: [a] -> integer   --a minuscola: tipo polimorfo, funziona per liste di ogni tipo
lunghezza [] = 0
lunghezza (_:xs) =1 + lunghezza xs
-- massimo di una lista
massimo :: [Integer]-> Integer
massimo[x]=x
massimo (x.xs) = max x (massimo xs)
--funzioni come valori 
applicaDueVolte :: (a->a) -> a -> a
applicaDueVolte f x = f (f x)
--(a->a)->a->a dice: prendo una funzione f (da a ad a) e un valore x, e restituisco f(f x)
-- parentesi attorno ad (a->a) indicano che è una funzione
-- funzione anonima \x -> corspo, usa-e-getta
--map applica una funzione a ogni elemento della lista
map :: (a->b)->[a]->[b]
map _ [] = []
map f (x:xs) = f x : map f xs
--filter trattiene elementi che soddisfano un predicato (restituisce Bool)
filter :: (a->Bool)->[a]->[a]
filter _ [] = []
filter p (x:xs)
| p x    = x : filter p xs
| otherwise = filter p xs
--[1,2,3] non è altro che 1 : (2 : (3 : []))
--foldr f z: sostituisce ogni : con f e il [] finale con z
foldr :: (a->b->b) -> b -> [a] -> b
foldr _ z []   = z
foldr f z (x:xs)  =  f x (foldr f z xs)
-- composizione funzioni (f . g) x significa f (g x)
(.) :: (b->c) -> (a->b) -> a -> c

-- somma solo numeri pari
sommaPari :: [Integer] -> Integer
sommaPari = sum . filter even
-- si legge da destra a sinistra
-- quadrati di tutti gli elementi
quadrati :: [Integer] -> [Integer]
quadrati xs = map (^2) xs
-- somma quadrati pari
sommaQuadratiPari :: [Integer] -> Integer
sommaQuadratiPari = sum . filter even . map (^2)
-- foldr1 prende due argomenti: niente valore iniziale. Al suo posto, usa l'ultimo elemento della
-- lista come caso base, caso base è con un elemento mai []
foldr1 _ [x] = x
foldr1 f (x:xs) = f x (foldr1 f xs)
-- tipi dato algebrici, data: nascono il tipo somma(scelta tra alternativa) e prodotto(aggregazione)
data Forma = Cerchio Double
           | Rettangolo Double Double
deriving Show
--Forma o è un cerchio o un rettangolo
area :: Forma -> Double
area (Cerchio r)       = p1 * r^2
area (Rettangolo b h) = b*h
-- calcola le aree di ogni tipo di Forma, avvisa se dimentichi casi
--un tipo può avere un parametro
data Maybe a = Nothing | Just a
--Un Maybe a è o Nothing (valore assente) o Just x (un valore x presente), 
-- risultato potrebbe non esistere
--massimo totale
massimoSicuro :: [Integer] -> Maybe Integer
massimoSicuro [] = Nothing
massimoSicuro xs = Just (foldrl max xs)
--massimo con lista vuota, descrizione:
descriviMax :: [Integer] -> String
descriviMax xs = case massimoSicuro xs of
  Nothing -> "lista vuota"
  Just m -> "il massimo è " ++ show m
-- albero binario, tipo riferirsi a sè stesso
data Albero a = Foglia
              | Nodo (Albero a) a (Albero a)
   deriving Show
--un nodo contiene sottoalbero sinistro, a, sottoalbero destro: ricorsione strutturale
dimensione :: Albero a -> Integer
dimensione Foglia      = 0
dimensione (Nodo sx _ dx) = 1 + dimensione sx + dimensione dx
--caso base sul costruttore non ricorsivo (Foglia), passo ricorsivo su (Nodo), con le chiamate
--sui sottoalberi sx e dx. Induzione strutturale tradotta in codice
--Anche la lista non ha nulla di speciale, semplicemente un tipo algebrico ricorsivo
data Lista a = Vuota | Cons a (Lista a)
--Vuota è [] e Cons è l'operatore :
--perimetro (come area)
perimetro :: Forma -> Double
perimetro (Cerchio r)  =2*pi*r
perimetro (Rettangolo b h) = (b+h)*2
--funzione semaforo, cosa fa? Con data definiamo i colori, con prossimo la funzione
data Semaforo = Rosso | Giallo | Verde
  deriving Show
prossimo :: Semaforo -> Semaforo
prossimo Rosso = Verde
prossimo Verde = Giallo
prossimo Giallo = Rosso
--funzione che somma i valori di tutti i Nodi di un albero
sommaAlbero :: Albero Integer -> Integer
sommaAlbero Foglia = 0
sommaAlbero (Nodo sx x dx)= x + sommaAlbero sx + sommaAlbero dx
--classi di tipo è un insieme di tipi che condividono un'interfaccia comune (interfaccia
--o meglio nozione algebrica di struttura dotata di certe operazioni)

-- => ciò che precede questo, è un vincolo
massimoLista :: Ord a => [a] -> a
massimoLista = foldr1 max
--"per ogni tipo a che appartiene alla classe Ord, questa funzione va da [a] ad a"
-- vincolo Ord a è ciò che autorizza l'uso di max, perchè max è un'operazione fornita proprio sulla
--classe Ord, senza vincolo il compilatore rifiuterebbe il programma

--4 Classi:
--Eq (uguaglianza, con == e /=)
--Ord (ordinamento, con compare, <, max, min)
--Show (conversione in stringa, con Show)
--Num (operazioni aritmetiche)

instance Eq Semaforo where
  Rosso == Rosso = True
  Giallo == Giallo = True
  Verde == Verde = True
_       == _     = False
--definito solo == ma anche /= funziona, perchè la classe Eq fornisce una 
--definizione di default
data Semaforo = Rosso | Giallo | Verde
  deriving (Eq, Ord, Show)
--Eq confronta strutturalmente; Show produce rappresentazione testuale
--Ord usa l'ordine di dichiarazione dei costruttori

--definire le proprie classi
class Misurabile a where
  dimensioneDi :: a-> Integer
  vuoto        :: a-> Bool
  vuoto x = dimensioniDi x == 0

--rendiamo Albero un'istanza
instance Misurabile (Albero a) where
  dimensioneDi Foglia       = 0
  dimensioneDi (Nodo sx _ dx) = 1 + dimensioneDi sx + dimensioneDi dx

--rendere forma instanza di Eq
instance Eq Forma where
Cerchio r==Cerchio r1 = r==r1
Rettangolo b h == Rettangolo b1 h1 = b==b1 && h==h1
 _         == _          = False
--classe descrivibile
class Descrivibile a where
descrivi :: a -> String --istanze di Forma e Semaforo
instance Descrivibile Forma where
descrivi (Cerchio _)= "Sono un cerchio"
descrivi (Rettangolo _) = "Sono un rettangolo"

instance Descrivibile Semaforo where
descrivi Rosso = "Traffico fermo"
descrivi Giallo = "Auto inizia a fermarsi"
descrivi Verde = "Traffico va avanti"

--tutteUguali se tutti elementi lista sono uguali
tutteUguali :: Eq a => [a] -> Bool
tutteUguali []= True
tutteUguali [_] = True
tutteUguali (x:xs) = x == head xs && tutteUguali xs

--haskell le liste infinite le ferma prima senza calcolare tutto quello dopo
--definire valori in termini di sè stessi
naturali :: [Integer]
naturali = 0 : map (+1) naturali

fibonacci :: [Integer]
fibonacci = 0 : 1 : zipWith (+) fibonacci (tail fibonacci)
--qua converge perchè ogni elemento viene prodotto solo quando richiesto

--foldr f z costituisce f x1 (f x2 (..)). Se f è pigra nel secondo argomento 
--foldr può lavorare ancge su liste infinite e terminare
--foldl deve consumare tutta la lista
--space leak quando crea un thunk che consuma memoria
--seq aiuta a forzare l'accumulatore
seq :: a -> b -> b
--seq x y forza x alla WHNF e poi restituisce y. 
--foldl' per accumulazioni su liste lunghe
--foldr giusto per operatore pigro o lista infinita
--undefined (bottom) abita una struttura senza farla divergere

--definire le potenze di due
potenzeDiDue :: [Integer]
potenzeDiDue=iterate (*2) 1
--quadratiNaturali con come map
naturali :: [Integer]
naturali = 0 : map (+1) naturali

quadratiNaturali :: [Integer]
quadratiNaturali = map (^2) naturali
