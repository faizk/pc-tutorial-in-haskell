module Fun.Utils
    ( Render(..)
    ) where

class Render a where
  render :: a -> String


