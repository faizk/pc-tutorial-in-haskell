{-# LANGUAGE GADTs #-}
module Fun.Json
    ( Json(..)
    , Key(..)
    ) where

import Test.QuickCheck
import Fun.Utils (GenShow(..))
import Data.List (intercalate)

data Key where
  Key :: String -> Key
  deriving (Eq)

data Json
  = Str String
  | Num Integer
  | Arr [Json]
  | Obj [(Key, Json)]
  | Bool Bool
  | Null
  deriving (Eq)

instance Show Json where
  show (Str s) = "\"" ++ s ++ "\""
  show (Num s) = show s
  show (Arr a) = show a
  show (Obj l) = "{" ++ intercalate "," (map (\(Key k,v) -> "\"" ++ k ++ "\": " ++ show v) l) ++ "}"
  show (Bool True) = "true"
  show (Bool False) = "false"
  show Null = "null"

instance GenShow Json where
  genShow j =
    case j of
      Num _ -> sur j
      Str _ -> sur j
      Null -> sur j
      Bool _ -> sur j
      Arr a -> braces "[" (intercalate "," <$> mapM genShow a) "]"
      Obj o -> braces "{" (intercalate "," <$> mapM f o) "}"
        where
          f (Key k, v) =
            do
              vs <- genShow v
              ks <- genShow (Str k)
              return $ ks ++ ":" ++ vs
    where
      braces a x z = concat <$> sequence [return a, x, return z]
      pn = choose (0, 4)
      space n = vectorOf n $ oneof $ map return " \t"
      sur :: Json -> Gen String
      sur j' =
        do
          l <- pn; r <- pn
          ls <- space l; rs <- space r
          return $ ls ++ show j' ++ rs

genAlhaNumChar :: Gen Char
genAlhaNumChar = oneof $ map return $ a ++ a' ++ n ++ s
  where
    a  = ['a'..'z']; a' = ['A'..'Z']
    n  = ['0'..'9']; s  = "-+/', @#"

genSmallStr :: Gen [Char]
genSmallStr = do
  k <- chooseInt (0, 8)
  vectorOf k genAlhaNumChar

instance Arbitrary Key where
  arbitrary = Key <$> genSmallStr

instance Arbitrary Json where
  arbitrary = sized arbitrary'
    where
      arbitrary' 0 =
        oneof [ return $ Arr [], return $ Obj [] ]
      arbitrary' n =
        oneof [ Str <$> genSmallStr
              , Num <$> arbitrary
              , Bool <$> arbitrary
              , Arr <$> resize (n `div` 2) arbitrary
              , Obj <$> resize (n `div` 2) arbitrary
              , return Null
        ]
