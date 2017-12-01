module MainProject where
htmlEscape :: String -> String
htmlEscape [] = []
htmlEscape (x:xs) = (case x of
    ' ' -> "%20"
    '(' -> "%28"
    ')' -> "%29"
    '{' -> "%7B"
    '}' -> "%7D"
    '+' -> "%2B"
    '-' -> "%2D"
    '^' -> "%5E"
    '_' -> "%5F"
    '.' -> "%2E"
    '\\' -> "%5C"
    _ | (isAlpha x) || (isDigit x) -> [x]
    _ -> error ("unknown character " ++ show x)
  ) ++  htmlEscape xs

-- latex render url

latex_URL :: Int -> String -> String
latex_URL sz tex = "http://www.texrendr.com/cgi-bin/mathtex.cgi?\\dpi{" ++
                    show (10 * sz) ++
                    "}" ++
                    (htmlEscape tex)
