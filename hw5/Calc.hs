data ExprT = Lit Integer
           | Add ExprT ExprT
           | Mul ExprT ExprT
    deriving (Show, Eq)

-- exercise 1: write the calculator: an evaluator for ExprT
eval :: ExprT -> Integer
eval (Mul (Lit a) (Lit b)) = a * b
eval (Add (Lit a) (Lit b)) = a + b
eval (Lit a) = a
eval (Mul exp1 exp2) = eval (Mul (Lit (eval exp1)) (Lit (eval exp2)))
eval (Add exp1 exp2) = eval (Add (Lit (eval exp1)) (Lit (eval exp2)))
