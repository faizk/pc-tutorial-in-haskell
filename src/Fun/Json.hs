{-# LANGUAGE GADTs #-}
module Fun.Json
    (Json(..)
    ) where

import Test.QuickCheck

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

commas :: [String] -> String
commas [] = ""
commas [s] = s
commas (s:ss) = s ++ "," ++ commas ss

instance Show Json where
  show (Str s) = "\"" ++ s ++ "\""
  show (Num s) = show s
  show (Arr a) = show a
  show (Obj l) = "{" ++ commas (map (\(Key k,v) -> "\"" ++ k ++ "\": " ++ show v) l) ++ "}"
  show (Bool True) = "true"
  show (Bool False) = "false"
  show Null = "null"


---

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

