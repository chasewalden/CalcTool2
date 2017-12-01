{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty

-- import MathUtil
main = scotty 8000 $ do
  get  "/" $ do
    html "<form method='post' action='/'><input type='submit' ></form>"
  post "/" $ do
    html "Posted"
