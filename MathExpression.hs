module MathExpression where

import Text.ParserCombinators.ReadP
import Data.Char(isAlpha, isDigit)

import Parser
import LaTeXWriter

data MathExpression =
  Constant Double |
  Symbol String |
  Variable Char |
  Subtract MathExpression MathExpression |
  Add MathExpression MathExpression |
  Divide MathExpression MathExpression |
  Multiply MathExpression MathExpression |
  Exponent MathExpression MathExpression |
  ParenthesisExpression MathExpression |
  MathFunction String MathExpression
  deriving (Show, Eq)

-- parser

instance ParserFor MathExpression where
  parser = skipSpaces *> parse_subtraction <* skipSpaces
    where

      parse_number = do
        sign  <- option "" (string "-")
        ipart <- munch1 isDigit
        fpart <- option "0" ((char '.') >> (munch1 isDigit))
        return ( Constant (read (sign ++ ipart ++ "." ++ fpart) :: Double))

      parse_variable = do
        var <- satisfy isAlpha
        return ( Variable var )

      operator op higher con = (do
        lhs <- higher
        skipSpaces
        char op
        skipSpaces
        rhs <- operator op higher con
        return (con lhs rhs)) <++ (higher)

        -- special case for multiplication to allow implicit multiplication i.e. 5x
      parse_multiplication = (do
        lhs <- parse_exponent
        explicit_mult <- option Nothing (fmap Just (skipSpaces *> char '*' <* skipSpaces))
        rhs <- parse_multiplication
        case (lhs, explicit_mult, rhs) of
          (_, Just '*', _) -> return (Multiply lhs rhs)
          (Constant _, Nothing, Constant _) -> pfail
          (_, Nothing, Constant a) | a < 0 -> pfail
          _ -> return (Multiply lhs rhs)

        ) <++ (parse_exponent)

      parse_exponent = operator '^' parse_parenthesis Exponent
      parse_division = operator '/' parse_multiplication Divide
      parse_addition = operator '+' parse_division Add
      parse_subtraction = operator '-' parse_addition Subtract

      parse_parenthesis = (do
        group <- between  (char '(' *> skipSpaces) (skipSpaces <* char ')') (parse_subtraction)
        return (ParenthesisExpression group)) <++ (parse_number +++ parse_variable)

-- to latex

instance ToLatex MathExpression where

  latexString (Constant c) =
    let ipart = truncate c
        fpart = c - (fromIntegral ipart)
    in case fpart of
      0 -> show ipart
      _ -> show c

  latexString (Variable x) = [x]

  latexString (Subtract lhs rhs) =
    (latexString lhs) ++ " - " ++ (latexString rhs)

  latexString (Add lhs rhs) =
    (latexString lhs) ++ " + " ++ (latexString rhs)

  latexString (Multiply lhs rhs) =
    let sep = case (lhs, rhs) of
                (Constant _, Constant _) -> " \\times "
                (_, _)-> ""
    in (latexString lhs) ++ sep ++ (latexString rhs)

  latexString (Divide lhs rhs) =
    "\\frac{" ++
    (latexString $ unwrap lhs) ++
    "}{" ++
    (latexString $ unwrap rhs) ++
    "}"
      where
        unwrap (ParenthesisExpression inner) = inner
        unwrap other = other

  latexString (Exponent lhs rhs) =
    (latexString lhs) ++
    "^{" ++
    (latexString $ unwrap rhs) ++
    "}"
      where
        unwrap (ParenthesisExpression inner) = inner
        unwrap other = other

  latexString (ParenthesisExpression inner) =
    "(" ++ (latexString inner) ++ ")"

  latexString (MathFunction name body) = name ++ (latexString (wrap body))
    where
      wrap body' @ (ParenthesisExpression _) = body'
      wrap body' = ParenthesisExpression body'
