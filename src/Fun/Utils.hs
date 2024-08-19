module Fun.Utils
    ( Render(..)
    , Rendering(..)
    , GenShow(..)
    , toEither
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

toEither :: e -> Maybe a -> Either e a
toEither e Nothing = Left e
toEither _ (Just a) = Right a
