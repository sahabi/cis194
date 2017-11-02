fun1 :: [Int] -> Int
fun1 = product . map (subtract 2) . filter even 

fun2 :: Int -> Int
fun2 n = sum lst where lst = takeWhile (/= 1) $ iterate (\x -> if even x then x `div` 2 else 3*x+1) n

data Tree a = Leaf
            | Node Int (Tree a) a (Tree a)
    deriving (Show, Eq, Ord)

foldTree :: (Ord a) => [a] -> Tree a
foldTree = foldr addElem Leaf 

addElem :: (Ord a) => a -> Tree a -> Tree a
addElem e Leaf = Node 0 Leaf e Leaf 
addElem e (Node n left root right) 
    | left > right = Node (treeLevel newRight + 1) left root newRight
    | otherwise    = Node (treeLevel newLeft + 1) newLeft root right
    where
        newRight = addElem e right
        newLeft  = addElem e left   

treeLevel :: Tree a -> Int
treeLevel Leaf = 0
treeLevel (Node n _ _ _) = n 



