module PlaintextWriter where

class ToPlaintext a where
  plaintext :: a -> String
