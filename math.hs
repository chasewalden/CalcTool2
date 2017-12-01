module MathUtil where

import Data.Fixed(mod')
import Parser

reduceMathExpression :: MathExpression -> MathExpression
reduceMathExpression (ParenthesisExpression expr) =
  case (reduceMathExpression expr) of
    c @ (Constant _) -> c
    v @ (Variable _) -> v
    other @ _ -> ParenthesisExpression other

reduceMathExpression (Add lhs rhs) =
  let slhs = reduceMathExpression lhs
      srhs = reduceMathExpression rhs
  in case (slhs, srhs) of
    (Constant 0, _) -> srhs
    (_, Constant 0) -> slhs
    (Constant a, Constant b) -> Constant (a+b)
    _ -> Add slhs srhs

reduceMathExpression (Subtract lhs rhs) =
  let slhs = reduceMathExpression lhs
      srhs = reduceMathExpression rhs
  in case (slhs, srhs) of
    (Constant 0, Constant a) -> (Constant $ negate a)
    (Constant 0, _) -> Multiply (Constant $ negate 1) srhs
    (_, Constant 0) -> slhs
    (Constant a, Constant b) -> Constant (a-b)
    _ -> Subtract slhs srhs

reduceMathExpression (Multiply lhs rhs) =
  let slhs = reduceMathExpression lhs
      srhs = reduceMathExpression rhs
  in case (slhs, srhs) of
    (Constant 0, _) -> Constant 0
    (_, Constant 0) -> Constant 0
    (Constant 1, _) -> srhs
    (_, Constant 1) -> slhs
    (Constant a, Constant b) -> Constant (a*b)
    _ -> Multiply slhs srhs

reduceMathExpression (Divide lhs rhs) =
  let slhs = reduceMathExpression lhs
      srhs = reduceMathExpression rhs
  in case (slhs, srhs) of
    (Constant 0, _) -> Constant 0
    (_, Constant 0) -> error "Divide by zero"
    (_, Constant 1) -> slhs
    (Constant a, Constant b) | (mod' a b) == 0.0 -> Constant (a/b)
    _ -> Divide slhs srhs

reduceMathExpression (Exponent lhs rhs) =
  let slhs = reduceMathExpression lhs
      srhs = reduceMathExpression rhs
  in case (slhs, srhs) of
    (_, Constant 0) -> Constant 1
    (Constant 0, _) -> Constant 0
    (Constant 1, _) -> Constant 1
    (_, Constant 1) -> slhs
    (Constant a, Constant b) -> Constant (a ** b)
    _ -> Exponent slhs srhs

reduceMathExpression other = other
