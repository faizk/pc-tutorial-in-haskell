module Fun.PC1.Calc
    ( calcP
    , exprP
    , eval
    ) where

import Fun.PC1

data Expr
  = Const Int
  | Sum Expr Expr
  | Prod Expr Expr
  | Minus Expr Expr
  | Div Expr Expr
  deriving Show

op2 :: Parser a -> Parser (a -> a -> a) -> Parser a -> Parser a
op2 lp opp rp = pmap f' $ lp `foll` ws opp `foll` rp
  where f' ((a, f), b) = f a b

cp :: Char -> (a -> a -> a) -> Parser (a -> a -> a)
cp c f = const f `pmap` ws (char c)

intP :: Parser Int
intP = pmap fromIntegral integerP

parens :: Parser a -> Parser a
parens p = surr2 (char '(') p (char ')')

calcP :: Parser Int
calcP = subP
  where
    subP   = op2 addP  (cp '-' (-)) subP  `orElse` addP
    addP   = op2 multP (cp '+' (+)) addP  `orElse` multP
    multP  = op2 divP  (cp '*' (*)) multP `orElse` divP
    divP   = op2 bp    (cp '/' div) divP  `orElse` bp
    bp     = ws $ parens $ subP `orElse` intP

exprP :: Parser Expr
exprP = subP
  where
    subP   = op2 addP  (cp '-' Minus) subP  `orElse` addP
    addP   = op2 multP (cp '+' Sum)   addP  `orElse` multP
    multP  = op2 divP  (cp '*' Prod)  multP `orElse` divP
    divP   = op2 bp    (cp '/' Div)   divP  `orElse` bp
    bp     = ws $ parens $ subP `orElse` pmap Const intP

eval :: Expr -> Int
eval (Const n) = n
eval (Sum a b) = eval a + eval b
eval (Minus a b) = eval a - eval b
eval (Prod a b) = eval a * eval b
eval (Div a b) = eval a `div` eval b
