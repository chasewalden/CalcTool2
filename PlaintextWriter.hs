module PlaintextWriter where

  

write_MathExpression :: MathExpression -> String
write_MathExpression expr = case expr of
  (Constant c) ->  show c
  (Variable x) -> [x]
  (Subtract lhs rhs) ->
    (write_MathExpression lhs) ++ " - " ++ (write_MathExpression rhs)
  (Add lhs rhs) ->
    (write_MathExpression lhs) ++ " + " ++ (write_MathExpression rhs)
  (Multiply lhs rhs) ->
    (write_MathExpression lhs) ++ " * " ++ (write_MathExpression rhs)
  (Divide lhs rhs) ->
    (write_MathExpression lhs) ++ " / " ++ (write_MathExpression rhs)
  (Exponent lhs rhs) ->
    (write_MathExpression lhs) ++ "^" ++ (write_MathExpression rhs)
  (ParenthesisExpression inner) ->
    "(" ++ (write_MathExpression inner) ++ ")"
  (MathFunction name body) -> name ++ (write_MathExpression (wrap body))
  where
    wrap body' @ (ParenthesisExpression _) = body'
    wrap body' = ParenthesisExpression body'
