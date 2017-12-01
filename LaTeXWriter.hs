module LaTeXWriter where

import Data.Char(isAlpha, isDigit, isAscii)
import Control.Monad
import Text.Printf


valid c = (isAlpha c) || (isDigit c) || c `elem` "-_.~"
htmlEscape' c
  | valid c = return [c]
  | isAscii c = return (printf "%%%02X" c)
  | otherwise = fail (printf "Invalid character for URI: '%c'" c)

htmlEscape :: (Monad m) => String -> m String
htmlEscape input = do
    cs <- (mapM htmlEscape' input)
    return $ concat cs


class ToLatex a where
  latexString :: a -> String

  latexURL    :: (Monad m) => Int -> a -> m String
  latexURL sz o = do
      latex <- htmlEscape (latexString o)
      return ("http://www.texrendr.com/cgi-bin/mathtex.cgi?\\dpi{" ++ show (10 * sz) ++ "}" ++ latex)
