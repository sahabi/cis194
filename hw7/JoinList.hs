{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
import Sized
import Data.Monoid
import Scrabble
--import StringBuffer
--import Buffer
--import Editor
sizeJoinList =
  Append (Size 3)
      (Append (Size 2)
        (Single (Size 1) "hi")
        (Single (Size 1) "bye")
      )
     (Single (Size 1) "tschau")
someJoinList =
  Append (Product 210)
    (Append (Product 30)
      (Single (Product 5) 'y')
      (Append (Product 6)
        (Single (Product 2) 'e')
        (Single (Product 3) 'a')))
    (Single (Product 7) 'h')
-- Exercise 1
data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) x y = Append (tag x <> tag y) x y
infixr 5 +++

tag :: Monoid m => JoinList m a -> m
tag jl = case jl of
  Empty        -> mempty
  Single m _   -> m
  Append m _ _ -> m

-- exercise 2
jlToList :: JoinList m a -> [a]
jlToList Empty          = []
jlToList (Single _ a) = [a]
jlToList (Append _ a b) = jlToList a ++ jlToList b

-- | Finds the JoinList element at the specified index.
-- | (indexJ i jl) == (jlToList jl !!? i)
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ index (Single _ a)
  | index == 0 = Just a
  | otherwise  = Nothing
indexJ index (Append m l1 l2)
  | index < 0 || index > size0 = Nothing
  | index < size1              = indexJ index l1
  | otherwise                  = indexJ (index - size1) l2
    where size0 = getSize . size $ m
          size1 = getSize . size . tag $ l1
indexJ _ _ = Nothing

dropJ :: (Sized b, Monoid b) =>
        Int -> JoinList b a -> JoinList b a
dropJ n jl
    | n == 0 = jl
    
dropJ n (Single a b)
    | n == 0 = Single a b
    | otherwise = Empty
dropJ n (Append m l1 l2)
    | n > size0 = Empty
    | n >= size1 = dropJ (n - size1) l2
    | otherwise = dropJ n l1 +++ l2
      where size0 = getSize . size $ m
            size1 = getSize . size. tag $ l1
dropJ _ _ = Empty

takeJ :: (Sized b, Monoid b) =>
        Int -> JoinList b a -> JoinList b a
takeJ n (Single a b)
    | n > 0 = Single a b
takeJ n jl@(Append m l1 l2)
    | n > size0 = jl
    | n < size1 = takeJ n l1
    | n >= size1 = l1 +++ takeJ (n - size1) l2
      where size0 = getSize . size $ m
            size1 = getSize . size. tag $ l1
takeJ _ _ = Empty

-- exercise 3

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s
