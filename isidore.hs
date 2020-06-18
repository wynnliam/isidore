-- Liam Wynn, 6/18/2020, Isidore

{-|
	Isidore is a simple calculator for arithmetic expressions.
	It will support addition, subtraction, multiplication,
	and division of simple integer numbers.

	The idea is that you can see how the evaluator computes
	each step.
-}

data Term = Const Int | Add Term Term | Sub Term Term | Mul Term Term | Div Term Term deriving (Show)
data Output = Exception String | Result Int deriving (Show)
type State = (String, Output)

eval :: Term -> State
eval (Const a) = ((line (Const a) (Result a)), (Result a))
eval (Div x y) =
  let a = eval x
      b = eval y
      res = tryOp (snd a) (snd b) (div)
  in ((fst a) ++ (fst b) ++ (line (Div x y) res), res)

eval (Mul x y) =
  let a = eval x
      b = eval y
      res = tryOp (snd a) (snd b) (*)
  in ((fst a) ++ (fst b) ++ (line (Mul x y) res), res)

eval (Add x y) =
  let a = eval x
      b = eval y
      res = tryOp (snd a) (snd b) (+)
  in ((fst a) ++ (fst b) ++ (line (Add x y) res), res)

eval (Sub x y) =
  let a = eval x
      b = eval y
      res = tryOp (snd a) (snd b) (-)
  in ((fst a) ++ (fst b) ++ (line (Add x y) res), res)

line :: Term -> Output -> String
line t q = (show t) ++ " = " ++ show q ++ "\n"
--eval (Add x y) = tryOp (eval x) (eval y) (+)
--eval (Sub x y) = tryOp (eval x) (eval y) (-)
--eval (Mul x y) = tryOp (eval x) (eval y) (*)
--  
tryOp :: Output -> Output -> (Int -> Int -> Int) -> Output
tryOp (Exception p) _ _ = Exception p
tryOp _ (Exception p) _ = Exception p
tryOp (Result 0)  _ (div) = Exception "Divided by zero"
tryOp _ (Result 0) (div) = Exception "Divided by zero"
tryOp (Result x) (Result y) op = Result (op x y)
