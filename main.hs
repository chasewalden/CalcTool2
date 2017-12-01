module Main where

import Web.Scotty

-- import MathUtil
main = scotty 8000 $ do
  get "/" $ do
    html "Hello World"
