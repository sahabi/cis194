data ExprT = Lit Integer
           | Add ExprT ExprT
           | Mul ExprT ExprT
    deriving (Show, Eq)

-- exercise 1: write the calculator: an evaluator for ExprT
eval :: ExprT -> Integer
eval (Mul a b) = eval a * eval b
eval (Add a b) = eval a + eval b
eval (Lit a) = a
