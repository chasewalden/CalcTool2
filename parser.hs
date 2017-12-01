module Parser where

import Text.ParserCombinators.ReadP

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

isDigit :: Char -> Bool
isDigit char = char >= '0' && char <= '9'

isAlpha :: Char -> Bool
isAlpha char = (char >= 'a' && char <= 'z') || (char >= 'A' && char <= 'Z')

parse_number = do
  sign  <- option "" (string "-")
  ipart <- munch1 isDigit
  fpart <- option "0" ((char '.') >> (munch1 isDigit))
  return ( Constant (read (sign ++ ipart ++ "." ++ fpart) :: Double))

parse_variable = do
  var <- satisfy isAlpha
  return ( Variable var )

operator ::
  Char ->
  ReadP MathExpression ->
  (MathExpression -> MathExpression -> MathExpression) ->
  ReadP MathExpression

operator op higher con = (do
  lhs <- higher
  skipSpaces
  char op
  skipSpaces
  rhs <- operator op higher con
  return (con lhs rhs)) <++ (higher)

parse_parenthesis = (do
  group <- between  (char '(' *> skipSpaces) (skipSpaces <* char ')') (parse_subtraction)
  return (ParenthesisExpression group)) <++ (parse_number +++ parse_variable)

-- parse_function =

parse_exponent = operator '^' parse_parenthesis Exponent

  -- special case for multiplication to allow implicit multiplication i.e. 5x
parse_multiplication = (do
  lhs <- parse_exponent
  explicit_mult <- option Nothing (fmap Just (skipSpaces *> char '*' <* skipSpaces))
  rhs <- parse_multiplication
  case (lhs, explicit_mult, rhs) of
    (_, Just '*', _) -> return (Multiply lhs rhs)
--  (Variable _, Nothing, Variable _) -> pfail
    (Constant _, Nothing, Constant _) -> pfail
    (_, Nothing, Constant a) | a < 0 -> pfail
    _ -> return (Multiply lhs rhs)

  ) <++ (parse_exponent)

parse_division = operator '/' parse_multiplication Divide
parse_addition = operator '+' parse_division Add
parse_subtraction = operator '-' parse_addition Subtract

parse_MathExpression = skipSpaces *> parse_subtraction <* skipSpaces

evaluate_input input = case (readP_to_S (parse_MathExpression <* eof) input) of
  [] -> error "Invalid input. Could not parse."
  [(x, "")] -> x
  [(_, rest)] -> error ("Could not parse \"" ++ rest ++ "\"")
  res @ (_:_) -> error ("Ambiguous input" ++ concatMap (\x -> "\n\t" ++ (show x)) res)
