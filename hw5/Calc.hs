{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

import Parser
import StackVM
import qualified Data.Map as M
import Data.Maybe

data ExprT = Lit Integer
           | Add ExprT ExprT
           | Mul ExprT ExprT
    deriving (Show, Eq)

-- exercise 1: write the calculator: an evaluator for ExprT
eval :: Main.ExprT -> Integer
eval (Main.Mul a b) = eval a * eval b
eval (Main.Add a b) = eval a + eval b
eval (Main.Lit a) = a

--exercise 2: 
evalStr :: String -> Maybe Integer
evalStr = fmap eval . Parser.parseExp Main.Lit Main.Add Main.Mul

--exercise 3: write a type class called Expr with three methods called lit, add, mul.
class Expr a where
  lit :: Integer -> a
  mul :: a -> a -> a
  add :: a -> a -> a

reify :: ExprT -> ExprT
reify = id

instance Expr ExprT where
  mul = Main.Mul
  lit = Main.Lit
  add = Main.Add 

--exercise 4:
instance Expr Integer where
  mul = (*)
  add = (+)
  lit = id

instance Expr Bool where
  lit x
    | x <= 0 = False
    | otherwise = True 
  mul = (&&)
  add = (||)

newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
  add (MinMax a) (MinMax b) = MinMax (max a b)
  mul (MinMax a) (MinMax b) = MinMax (min a b)
  lit = MinMax

newtype Mod7 = Mod7 Integer deriving (Eq, Show)
instance Expr Mod7 where
  add (Mod7 a) (Mod7 b) = Mod7 ((a + b) `mod` 7)
  mul (Mod7 a) (Mod7 b) = Mod7 ((a * b) `mod` 7)
  lit = Mod7

-- exercise 5: implement a compiler for arithmetic expressions
instance Expr StackVM.Program where
  lit a = [StackVM.PushI a]
  add a b = a ++ b ++ [StackVM.Add]
  mul a b = a ++ b ++ [StackVM.Mul]
 

compile :: String -> Maybe StackVM.Program
compile = parseExp lit add mul 

-- exercise 6
class HasVars a where
  var :: String -> a

data VarExprT = VLit Integer
              | VVar Integer
              | VAdd VarExprT VarExprT
              | VMul VarExprT VarExprT
  deriving (Show, Eq)

instance Expr VarExprT where
  lit = VLit
  add = VAdd
  mul = VMul

instance HasVars VarExprT where
  var x = VVar 42

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit a = (\_ -> Just a)
  add a b = \vs -> Just (fromJust (a vs) +  fromJust (b vs))
  mul a b = \vs -> Just (fromJust (a vs) * fromJust (b vs))

withVars :: [(String, Integer)]
         -> (M.Map String Integer -> Maybe Integer)
         -> Maybe Integer
withVars vs exp = exp $ M.fromList vs


exercise6 = do
  print $ (withVars [("x", 6)] $ add (lit 3) (var "x")) == Just 9
  print $ (withVars [("x", 6)] $ add (lit 3) (var "y")) == Nothing
  print $ (withVars [("x", 6), ("y", 3)]
    $ mul (var "x") (add (var "y") (var "x"))) == Just 54

-- tests
testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger :: Maybe Integer
testInteger = testExp

testBool :: Maybe Bool
testBool = testExp

testMM :: Maybe MinMax
testMM = testExp

testSat :: Maybe Mod7
testSat = testExp

testProg :: Maybe StackVM.Program
testProg = testExp
