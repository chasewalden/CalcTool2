module LaTeXWriter where

import Data.Char(isAlpha, isDigit, isAscii)
import Control.Monad
import Text.Printf


valid c = (isAlpha c) || (isDigit c) || c `elem` "-_.~"
htmlEscape' c
  | valid c = Right [c]
  | isAscii c = Right (printf "%%%02X" c)
  | otherwise = Left (printf "Invalid character for URI: '%c'" c)

htmlEscape :: String -> Either String String
htmlEscape input = do
    cs <- (mapM htmlEscape' input)
    return $ concat cs

class ToLatex a where
  latexString :: a -> String

  latexURL    :: Int -> a -> Either String String
  latexURL sz o = do
      latex <- htmlEscape (latexString o)
      return ("http://www.texrendr.com/cgi-bin/mathtex.cgi?\\dpi{" ++ show (10 * sz) ++ "}" ++ latex)
