module Fun.Json
    (Json(..)
    ) where

data Json
  = Str String
  | Num Integer
  | Arr [Json]
  | Obj [(String, Json)]
  | Bool Bool
  | Null

commas :: [String] -> String
commas [] = ""
commas [s] = s
commas (s:ss) = s ++ "," ++ commas ss

instance Show Json where
  show (Str s) = "\"" ++ s ++ "\""
  show (Num s) = show s
  show (Arr a) = show a
  show (Obj l) = "{" ++ commas (map (\(k,v) -> "\"" ++ k ++ "\": " ++ show v) l) ++ "}"
  show (Bool True) = "true"
  show (Bool False) = "false"
  show Null = "null"

