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

type Op = Int -> Int -> Int

op2 :: Parser Int -> Parser Op -> Parser Int -> Parser Int
op2 lp opp rp = pmap f' $ lp `foll` ws opp `foll` rp
  where f' ((a, f), b) = f a b

calcP :: Parser Int
calcP = subP
  where
    subP   = op2 addP  (opp '-' (-)) subP  `orElse` addP
    addP   = op2 multP (opp '+' (+)) addP  `orElse` multP
    multP  = op2 divP  (opp '*' (*)) multP `orElse` divP
    divP   = op2 bp    (opp '/' div) divP  `orElse` bp
    bp     = ws (parens subP `orElse` constP)
    constP = pmap fromIntegral integerP
    opp c f = const f `pmap` ws (char c)
    parens p = surr2 (char '(') p (char ')')
