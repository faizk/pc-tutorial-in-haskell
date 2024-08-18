module Fun.PC1.Calc
    (
    calcP
    ) where

import Fun.PC1

data Expr
  = Const Int
  | Sum Expr Expr
  | Prod Expr Expr
  | Minus Expr Expr
  | Div Expr Expr
  deriving Show

op :: Parser Int -> Char -> Parser Int -> (Int -> Int -> Int) -> Parser Int
op lp c rp f = pmap f' $ lp `foll` ws (char c) `foll` rp
  where f' ((a, _), b) = f a b

calcP :: Parser Int
calcP = subP
  where
    subP = op addP '-' subP (-) `orElse` addP
    addP = op multP '+' addP (+) `orElse` multP
    multP = op divP '*' multP (*) `orElse` divP
    divP = op ep '/' divP div `orElse` ep
    ep = ws (parens subP `orElse` constP)
    constP = pmap fromIntegral integerP
    parens p = surr2 (char '(') p (char ')')
    
