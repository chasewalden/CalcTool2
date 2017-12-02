module MathUtil where

import MathExpression

derive :: MathExpression -> Char -> MathExpression

derive (Constant a) _ = (Constant 0)

derive (Variable x) y
  | x == y = (Constant 1)
  | otherwise = (Constant 0)

derive (Add l r) v =
  let lhs = (derive l v)
      rhs = (derive r v)
  in case (lhs, rhs) of
    (Constant 0, _) -> rhs
    (_, Constant 0) -> lhs
    (Constant a, Constant b) -> (Constant (a + b))
    _ -> (Add lhs rhs)

derive (Subtract l r) v =
  let lhs = (derive l v)
      rhs = (derive r v)
  in case (lhs, rhs) of
    (Constant 0, Constant a) -> ( Constant ((0-1)*a) )
    (Constant 0, _) -> Multiply (Constant (0-1)) rhs
    (_, Constant 0) -> lhs
    (Constant a, Constant b) -> (Constant (a - b))
    _ -> (Subtract lhs rhs)

derive (Multiply l r) v =
  let lhs = (derive l v)
      rhs = (derive r v)
  in case (lhs, rhs) of
    _ -> (Add (Multiply lhs r) (Multiply rhs l))

derive (Divide l r) v =
  let lhs = (derive l v)
      rhs = (derive r v)
  in case (lhs, rhs) of
    _ -> (Divide (Subtract (Multiply lhs r) (Multiply rhs l)) (Exponent r (Constant 2)))

derive (Exponent l r) v =
  let ex = Multiply (MathFunction "ln" l) r
  in
  (Multiply (Exponent l r) (derive ex v))



derive a _ = a

-- integrate :: MathExpression -> Char -> MathExpression
