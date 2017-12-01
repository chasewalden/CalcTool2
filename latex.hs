module LaTeXWriter where

latex_MathExpression :: MathExpression -> String
latex_MathExpression expr = case expr of
  (Constant c) -> let ipart = truncate c
                      fpart = c - (fromIntegral ipart)
                  in case fpart of
                    0 -> show ipart
                    _ -> show c
  (Variable x) -> [x]
  (Subtract lhs rhs) ->
    (latex_MathExpression lhs) ++ " - " ++ (latex_MathExpression rhs)
  (Add lhs rhs) ->
    (latex_MathExpression lhs) ++ " + " ++ (latex_MathExpression rhs)
  (Multiply lhs rhs) ->
    let sep = case (lhs, rhs) of
                (Constant _, Constant _) -> " \\times "
                (_, _)-> ""
    in (latex_MathExpression lhs) ++ sep ++ (latex_MathExpression rhs)
  (Divide lhs rhs) ->
    "\\frac{" ++
    ((latex_MathExpression . unwrap) lhs) ++
    "}{" ++
    ((latex_MathExpression . unwrap) rhs) ++
    "}"
  (Exponent lhs rhs) ->
    (latex_MathExpression lhs) ++
    "^{" ++
    ((latex_MathExpression . unwrap) rhs) ++
    "}"
  (ParenthesisExpression inner) ->
    "(" ++ (latex_MathExpression inner) ++ ")"
  (MathFunction name body) -> name ++ (write_MathExpression (wrap body))
  where
    wrap body' @ (ParenthesisExpression _) = body'
    wrap body' = ParenthesisExpression body'
    unwrap (ParenthesisExpression inner) = inner
    unwrap other = other
