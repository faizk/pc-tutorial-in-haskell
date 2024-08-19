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

instance GenShow Key where genShow (Key k) = genShow (Str k)

instance GenShow Json where
  genShow j =
    case j of
      Arr a -> braces "[" (intercalate "," <$> mapM genShow a) "]"
      Obj o -> braces "{" (intercalate "," <$> mapM genKv o) "}"
      _ -> sur j
    where
      genKv :: (Key, Json) -> Gen String
      genKv (k, v) = (++) . (++ ":") <$> genShow k <*> genShow v
      braces a x z = concat <$> sequence [return a, x, return z]
      space = choose (0,4) >>= (`vectorOf` oneof (map return " \t"))
      sur :: Json -> Gen String
      sur j' = (++) . (++ show j') <$> space <*> space

genAlhaNumChar :: Gen Char
genAlhaNumChar = oneof $ map return $ a ++ a' ++ n ++ s
  where
    a  = ['a'..'z']; a' = ['A'..'Z']
    n  = ['0'..'9']; s  = "-+/', @#"

genSmallStr :: Gen [Char]
genSmallStr = choose (0, 8) >>= (`vectorOf` genAlhaNumChar)

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
