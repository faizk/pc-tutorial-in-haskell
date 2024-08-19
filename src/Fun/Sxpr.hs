module Fun.Sxpr
    ( Sxpr(..)
    , Val(..)
    ) where

import Fun.Utils
import Test.QuickCheck
import Data.List (intersperse)

data Val
  = Num Int
  | Str String
  | T | F
  deriving (Eq)

data Sxpr
  = Nil
  | Lit Val
  | Sym String
  | Sxpr :~ Sxpr
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
  h :~ t -> (h :) <$> maybeList t
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
    l :~ r -> "(" ++ inside ++ ")"
      where
        inside = case maybeList e of
          Just list -> unwords $ map render list
          Nothing -> render l ++ " . " ++ render r

instance GenShow Sxpr where
  genShow e = concat <$> sequence gens
    where
      space u v = choose (u, v) >>= (`vectorOf` oneof (map return " \t"))
      gens = case e of
        l :~ r -> space 0 2 : return "(" : inside ++ [return ")", space 0 2]
          where inside = case maybeList e of
                  Just list -> intersperse (space 1 8) $ map genShow list
                  Nothing -> [genShow l, return " . ", genShow r]
        _ -> [space 0 4, return $ render e, space 0 4]

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
        return $ l :~ r
      genList n = do
        v <- vectorOf (n `div` 2) (arbitrary :: Gen Sxpr)
        return $ foldl (:~) Nil v
      genSmallStr u v = do
        l <- choose (u, v)
        vectorOf l genAlphaNumChar
      genAlphaNumChar = elements ['a'..'z']
