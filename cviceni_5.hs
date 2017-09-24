module Cviceni_5 where

data Expr = Num Int
 | Add Expr Expr
 | Sub Expr Expr
 | Mul Expr Expr
 | Div Expr Expr
 | Var Char
 deriving (Eq)
 
expr1 = Mul(Add (Var 'x') (Num 2)) (Mul (Num 3) (Var 'x'))

eval :: Expr -> Int
eval (Num x) =  x 
eval (Add x y) = (eval x) + (eval y)
eval (Mul x y) = (eval x) * (eval y)
eval (Sub x y) = (eval x) - (eval y)
eval (Div x y) = (eval x) `div` (eval y)

showExpr :: Expr -> String
showExpr x =  tmp x 0 where 
  tmp (Num x) _ = show x
  tmp (Var x) _ = [x]
  tmp (Add x y) p = let s = (tmp x 1) ++"+"++ (tmp y 1)
                    in if p > 1 then "(" ++ s ++ ")" else s
  tmp (Mul x y) _ = (tmp x 2) ++"*"++ (tmp y 2)
  tmp (Sub x y) p = let s = (tmp x 1) ++"-"++ (tmp y 1)
                    in if p > 1 then "(" ++ s ++ ")" else s
  tmp (Div x y) _ = (tmp x 2) ++"/"++ (tmp y 2)
  
deriv :: Expr -> Char -> Expr
deriv (Num x) _ =  Num 0
deriv (Var x) y | x == y = Num 1
                | otherwise = Num 0
deriv (Add x y) z = Add (deriv x z) (deriv y z)
deriv (Mul x y) z = Add (Mul (deriv x z) y) (Mul x (deriv y z))
deriv (Sub x y) z = Sub (deriv x z) (deriv y z)
deriv (Div x y) z = Div (Sub (Mul (deriv x z) y) (Mul x (deriv y z))) (Mul y y)



