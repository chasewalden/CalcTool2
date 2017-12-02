module CommandExpression where

  import Text.ParserCombinators.ReadP

  import Parser
  import LaTeXWriter
  import PlaintextWriter

  import MathExpression

  import MathUtil

  data CommandExpression =
    IntegralOf MathExpression Char (Maybe (Int, Int)) |
    DerivativeOf MathExpression Char

    deriving(Show, Eq)

  instance ParserFor CommandExpression where
    parser = skipSpaces *> (choice [parse_derivative, parse_integral])  <* skipSpaces
      where
        parse_integral = do
          (string "integral of ") +++ (string "integrate ")
          expr <- parser :: ReadP MathExpression
          skipSpaces
          (char ' ')
          (char 'd')
          respectTo <- get
          return (IntegralOf expr respectTo Nothing)

        parse_derivative = ( do
              (string "derivative of ") +++ (string "derive ")
              expr <- parser :: ReadP MathExpression
              (string " with respect to ")
              respectTo <- get
              return (DerivativeOf expr respectTo)
            ) +++ ( do
              (string "d/d")
              respectTo <- get
              (char ' ')
              expr <- parser :: ReadP MathExpression
              return (DerivativeOf expr respectTo)
            )

  instance ToLatex CommandExpression where
    latexString (IntegralOf expr v range) =
      -- let integS = case range of
      --       Nothing -> "\\int"
      --       Just (a,b) -> ( "\\int_" ++ (show a) ++ "^" ++ (show b) )
          let intR = case range of
                Nothing -> ""
                Just (a,b) -> "_" ++ (show a) ++ "^" ++ (show b)
          in
            "\\int" ++ intR ++ "\\!" ++ (latexString expr) ++ "\\,\\mathrm{d}" ++ [v]

    latexString (DerivativeOf expr v) =
      "\\frac{\\mathrm{d}}{\\mathrm{d}" ++ [v] ++ "}\\ " ++ (latexString expr) ++
      "\\ = \\ " ++ (latexString (derive expr v))
