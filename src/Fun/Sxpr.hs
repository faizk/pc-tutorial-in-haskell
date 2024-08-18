module Fun.Sxpr
    ( Sxpr(..)
    , Val(..)
    ) where

import Data.Maybe (fromMaybe)
import Fun.Utils
import Test.QuickCheck

data Val
  = Num Int
  | Str String
  | T | F
  deriving (Eq)

data Sxpr
  = Nil
  | Lit Val
  | Sym String
  | Pair Sxpr Sxpr
  | Qt Sxpr
  | Qqt Sxpr
  | Uqt Sxpr
  deriving (Eq)

instance Show Val where show = render
instance Show Sxpr where show = render

instance Render Val where
  render (Num n) = show n
  render (Str s) = show s
  render T = "#t"
  render F = "#f"

maybeList :: Sxpr -> Maybe [Sxpr]
maybeList e = case e of
  Pair h t -> (h :) <$> maybeList t
  Nil -> pure []
  _ -> Nothing

instance Render Sxpr where
  render e = case e of
    Nil -> "()"
    Lit v -> render v
    Sym s -> s
    Qt s -> '\'' : render s
    Qqt s -> '`' : render s
    Uqt s -> ',' : render s
    Pair l r -> "(" ++ fromMaybe pair maybeLst ++ ")"
      where
        maybeLst = unwords . map render <$> maybeList e
        pair = render l ++ " . " ++ render r

instance Arbitrary Val where
  arbitrary = oneof [ Num <$> arbitrary
                    , return T, return F
                    ]

instance Arbitrary Sxpr where
  arbitrary = sized arbitrary'
    where
      arbitrary' 0 = return Nil
      arbitrary' n =
        oneof [ Sym <$> genSmallStr 1 8
              , Lit <$> arbitrary
              , Qt <$> resize (n `div` 2) arbitrary
              , Qqt <$> resize (n `div` 2) arbitrary
              , Uqt <$> resize (n `div` 2) arbitrary
              , genPair (n `div` 6)
              , genList (n `div` 8)
              ]
      genPair n = do
        l <- resize 1 arbitrary
        r <- resize ((n - 1) `max` 0) arbitrary
        return $ Pair l r
      genList n = do
        v <- vectorOf (n `div` 2) (arbitrary :: Gen Sxpr)
        return $ foldl Pair Nil v
      genSmallStr u v = do
        l <- choose (u, v)
        vectorOf l genAlphaNumChar
      genAlphaNumChar = elements ['a'..'z']
