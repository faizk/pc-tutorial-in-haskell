module Fun.Utils
    ( Render(..)
    , Rendering(..)
    , GenShow(..)
    , orL
    ) where

import Test.QuickCheck

class Render a where
  render :: a -> String

data Rendering a = Rendering a String
  deriving Show

class Arbitrary a => GenShow a where
  genShow :: a -> Gen String

instance GenShow a => Arbitrary (Rendering a) where
  arbitrary =
    do
      a <- arbitrary
      s <- genShow a
      return $ Rendering a s

orL :: Maybe a -> e -> Either e a
orL Nothing e = Left e
orL (Just a) _ = Right a
