{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

import Data.Monoid
--import StringBuffer
--import Buffer
--import Editor


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
