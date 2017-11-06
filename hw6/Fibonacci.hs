{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

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


-- exercise 4

streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons a b) = Cons (f a) (streamMap f b)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x (streamFromSeed f (f x))

-- exercise 5

nats :: Stream Integer
nats = streamFromSeed (+1) 0

streamInterleave :: Stream a -> Stream a -> Stream a
streamInterleave (Cons x xs) ys = Cons x (streamInterleave ys xs)

ruler :: Stream Integer
ruler = streamInterleave (streamRepeat 0) $ streamMap succ ruler

-- exercise 6
x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

instance Num (Stream Integer) where
    fromInteger n = Cons n (streamRepeat 0)
    negate (Cons y ys) = Cons (-y) (negate ys)
    (+) (Cons y ys) (Cons x xs) = Cons (y + x) (ys + xs)
    (*) (Cons y ys) b@(Cons x xs) = Cons (y * x) (streamMap (*y) xs + (ys * b))

instance Fractional (Stream Integer) where
    (/) a@(Cons y ys) b@(Cons x xs) =  q where q = Cons (y `div` x) (streamMap (`div` x) (ys - (q * xs))) 

fibs10 = x / (1 - x - x * x) 
