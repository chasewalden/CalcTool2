module Parser where

  import Text.ParserCombinators.ReadP

  class ParserFor a where
    parser :: ReadP a
    evaluate :: String -> Either String a
    evaluate input =
      case readP_to_S (parser <* eof) input of
        [] -> Left "Invalid input. Could not parse."
        [(x, "")] -> Right x
        [(_, rest)] -> Left (
            "Could not parse \"" ++ rest ++ "\""
          )
        res @ (_:_) -> Left ("Ambiguous input" ++
            concatMap (\(_,x) -> "\n\t" ++ x) res
          )
