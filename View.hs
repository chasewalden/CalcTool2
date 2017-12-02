{-# LANGUAGE OverloadedStrings #-}

module View where

  import Text.Blaze.Html
  import qualified Text.Blaze.Html5 as H
  import Text.Blaze.Html5.Attributes

  import MathExpression
  import CommandExpression

  import Parser
  import LaTeXWriter

  import qualified Data.Text as T


  page_template :: Maybe T.Text -> H.Html
  page_template a = H.html $ do
        H.head $ do
          H.title "CalcTool"
        H.body $ do
          H.h1 "Calculus Tool v2"
          H.form ! method "post" ! action "/" $ do
            H.input ! type_ "text" ! name "formula"
            H.input ! type_ "submit"
          case a of
            Nothing -> usage
            Just x -> let
              expr = evaluate (T.unpack x) :: Either String CommandExpression
              tex = expr >>= \e -> (latexURL 20 e)
              in case tex of
                Left err -> H.p $ H.toHtml (T.pack err)
                Right uri -> H.img ! src (textValue (T.pack uri))


  usage = H.p "Enter a command"
