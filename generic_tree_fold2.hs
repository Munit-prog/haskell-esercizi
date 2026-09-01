

--definizione

data (Eq a, Show a) => Tree a = Void | Node a [Tree a]
                                deriving (Eq, Show)

-- fold "base" (catamorfismo) per Tree

foldTree :: (Eq a, Show a) => (a -> [b] -> b) -> b -> Tree a -> b
foldTree _ z Void        = z
foldTree f z (Node x ts) = f x (map (foldTree f z) ts)



-- visita in preordine

preorder :: (Eq a, Show a) => Tree a -> [a]
preorder = foldTree (\x rs -> x : concat rs) []


--frontier

frontier :: (Eq a, Show a) => Tree a -> [a]
frontier = foldTree f []
  where f x rs = case concat rs of
                   [] -> [x]      -- nessun figlio ha prodotto nulla: x è una foglia
                   l  -> l        -- almeno un figlio è un sottoalbero vero

--smallParents

smallParents :: (Eq a, Show a) => Tree a -> [a]
smallParents t = res
  where (_, _, res) = foldTree f (False, False, []) t
        f x rs = (True, isParent, if isParent && not isGrand then x : below else below)
          where (nvs, ps, ls) = unzip3 rs
                isParent = or nvs        -- x ha almeno un figlio non Void
                isGrand  = or ps         -- almeno un figlio è a sua volta genitore
                below    = concat ls


--arithSmallParents

isArith :: (Eq a, Num a) => [a] -> Bool
isArith xs = null ds || all (== head ds) ds
  where ds = zipWith (-) (drop 1 xs) xs

arithSmallParents :: (Eq a, Show a, Num a) => Tree a -> Bool
arithSmallParents = isArith . smallParents


--rpn2tree

rpn2tree :: (Num a, Eq a, Show a, Eq op, Show op)
         => [Either a (op, Int)] -> Tree (Either a op)
rpn2tree = head . foldl push []
  where
    push st (Left  v)     = Node (Left v) [] : st
    push st (Right (o,n)) = let (args, rest) = splitAt n st
                            in Node (Right o) args : rest



