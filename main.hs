{-# LANGUAGE OverloadedStrings #-}

import Parser
import MathExpression

import Web.Scotty

import Text.Blaze.Html.Renderer.Text

import View

blaze = html . renderHtml

main = scotty 8000 $ do
  get "/" $ do
    blaze $ page_template Nothing
  post "/" $ do
    formula <- param "formula"
    blaze $ page_template (Just formula)
