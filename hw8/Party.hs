module Party where

import Employee
import Data.Monoid
import Data.Tree

-- exercise 1
glCons :: Employee -> GuestList -> GuestList
glCons e (GL gl fun)  = GL (e:gl) ((empFun e) + fun)

instance Monoid GuestList where
    mempty = GL [] 0
    (GL e1 f1) `mappend` (GL e2 f2) = GL (e1++e2) (f1+f2) 

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1@(GL _ f1) gl2@(GL _ f2)
        | f1 > f2   = gl1
        | otherwise = gl2

-- exercise 2
treeFold :: (b -> [a] -> a) -> Tree b -> a
treeFold f (Node root xs) = f root (map (treeFold f) xs)

-- exercise 3
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss sr = (wB, woB) where 
    wB = glCons boss $ mconcat (map fst sr)
    woB = mconcat (map (uncurry moreFun) sr) 

-- exercise 4
maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . (treeFold nextLevel) 

parse :: GuestList -> String
parse (GL emp fun) = "Fun score: "
                ++ show fun ++ "\n"
                ++ unlines (map empName emp) 

parseBest :: Tree Employee -> String
parseBest = parse . maxFun

main :: IO()
main = readFile "company.txt"
        >>= putStrLn . parseBest . read 
