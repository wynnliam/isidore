-- Liam Wynn, 6/18/2020, Isidore

{-|
	Isidore is a simple calculator for arithmetic expressions.
	It will support addition, subtraction, multiplication,
	and division of simple integer numbers.

	The idea is that you can see how the evaluator computes
	each step.
-}

data Term = Const Int | Add Term Term | Sub Term Term | Mul Term Term | Div Term Term
data Output = Exception String | Result Int deriving (Show)
--data State = State Output Term

eval :: Term -> Output
eval (Const a) = Result a
eval (Div x y) = tryOp (eval x) (eval y) (div)
eval (Add x y) = tryOp (eval x) (eval y) (+)
eval (Sub x y) = tryOp (eval x) (eval y) (-)
eval (Mul x y) = tryOp (eval x) (eval y) (*)
  
tryOp :: Output -> Output -> (Int -> Int -> Int) -> Output
tryOp (Exception p) _ _ = Exception p
tryOp _ (Exception p) _ = Exception p
tryOp (Result 0)  _ _ = Exception "Divided by zero"
tryOp _ (Result 0) _ = Exception "Divided by zero"
tryOp (Result x) (Result y) op = Result (op x y)
