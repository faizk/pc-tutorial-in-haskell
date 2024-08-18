module Fun.Sxpr
    ( Sxpr(..)
    , Val(..)
    ) where

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

instance Show Val where
  show (Num n) = show n
  show (Str s) = show s
  show T = "#t"
  show F = "#f"

renderList :: Sxpr -> (Bool, String)
renderList e = case e of
  Pair h t -> case renderList t of
    (True, "") -> (True,  show h)
    (True, s)  -> (True,  show h ++ " " ++ s)
    (False, s) -> (False, show h ++ " . " ++ s)
  Nil -> (True, "")
  _ -> (False, show e)

instance Show Sxpr where
  show e = case e of
    Nil -> "()"
    Lit v -> show v
    Sym s -> s
    Qt s -> '\'' : show s
    Qqt s -> '`' : show s
    Uqt s -> ',' : show s
    Pair _ _ -> "(" ++ s ++ ")"
      where
        (_, s) = renderList e

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
