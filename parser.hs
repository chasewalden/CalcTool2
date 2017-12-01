module Parser where
  
  import Text.ParserCombinators.ReadP

  class ParserFor a where
    parser :: ReadP a
    evaluate :: (Monad m) => String -> m a
    evaluate input =
      case readP_to_S (parser <* eof) input of
        [] -> fail "Invalid input. Could not parse."
        [(x, "")] -> return x
        [(_, rest)] -> fail (
            "Could not parse \"" ++ rest ++ "\""
          )
        res @ (_:_) -> fail ("Ambiguous input" ++
            concatMap (\(_,x) -> "\n\t" ++ x) res
          )
