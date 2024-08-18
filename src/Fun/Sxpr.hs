module Fun.Sxpr
    ( Sxpr(..)
    , Val(..)
    ) where

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

