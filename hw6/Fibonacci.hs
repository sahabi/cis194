-- exercise 1
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = [x | x <- map fib [0..]]

-- exercise 2
fibs2 :: [Integer]
fibs2 = 0 : 1 : zipWith (+) fibs2 (tail fibs2)


-- exercise 3
data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

streamFromList :: [a] -> Stream a
streamFromList (x:xs) = Cons x (streamFromList xs)

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList

