-- Liam Wynn, 6/18/2020, Isidore

{-|
	Isidore is a simple calculator for arithmetic expressions.
	It will support addition, subtraction, multiplication,
	and division of simple integer numbers.

	The idea is that you can see how the evaluator computes
	each step.
-}

data Term = Const Int | Add Term Term | Sub Term Term | Mul Term Term | Div Term Term deriving (Show)

eval :: Term -> IO Int
eval (Const a) = (line (Const a) a) >> (return a)
eval (Add x y) = (eval x) >>= (\p -> (eval y) >>= (\q -> (line (Add x y) (p + q)) >> (return (p + q))))
eval (Sub x y) = (eval x) >>= (\p -> (eval y) >>= (\q -> (line (Sub x y) (p - q)) >> (return (p - q))))
eval (Mul x y) = (eval x) >>= (\p -> (eval y) >>= (\q -> (line (Mul x y) (p * q)) >> (return (p * q))))
eval (Div x y) = (eval x) >>= (\p -> (eval y) >>= (\q -> (line (Div x y) (div p q)) >> (return (div p q))))

line :: Term -> Int -> IO ()
line t r = putStr ((show t) ++ " = " ++ (show r) ++ "\n")
