module Fun.Sxpr
    ( Sxpr(..)
    , Val(..)
    ) where

import Test.QuickCheck

data Val
  = Num Int
  | Str String
  | T | F
  deriving (Eq, Show)

data Sxpr
  = Nil
  | Lit Val
  | Sym String
  | Pair Sxpr Sxpr
  | Qt Sxpr
  | Qqt Sxpr
  | Uqt Sxpr
  deriving (Eq, Show)

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
              , Qt <$> resize (n - 1) arbitrary
              , Qqt <$> resize (n - 1) arbitrary
              , Uqt <$> arbitrary
              , genPair (n `div` 2)
              , genList (n `div` 4)
              ]
      genPair n = do
        l <- resize 1 arbitrary
        r <- resize ((n - 1) `max` 0) arbitrary
        return $ Pair l r
      genList n = do
        v <- vectorOf n (arbitrary :: Gen Sxpr)
        return $ foldl Pair Nil v
      genSmallStr u v = do
        l <- choose (u, v)
        vectorOf l genAlphaNumChar
      genAlphaNumChar = elements ['a'..'z']
