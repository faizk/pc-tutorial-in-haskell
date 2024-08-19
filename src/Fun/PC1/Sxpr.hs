module Fun.PC1.Sxpr
    ( sxprP
    ) where

import qualified Fun.Sxpr as S

import Fun.PC1
import Data.Char (isSpace)

zilchP :: Parser S.Sxpr
zilchP = pmap (const S.Nil) $ char '(' `foll` zeroOrMore wsP `foll` char ')'

symCP :: Parser Char
symCP = charP f
  where
    f c = case c of
      ',' -> False
      '`' -> False
      '\\' -> False
      '(' -> False
      ')' -> False
      _ | isSpace c -> False
      _   -> True

parensP :: Parser a -> Parser a
parensP p = pmap f $ char '(' `foll` ws p `foll` char ')'
  where f ((_, a), _) = a

symP :: Parser S.Sxpr
symP = S.Sym `pmap` oneOrMore symCP

cellP' :: Parser S.Sxpr
cellP' = pmap f $ ws sxprP `foll` dot `foll` sxprP
  where
    f ((l, _), r) = S.Pair l r
    dot = ws $ char '.'

listP' :: Parser S.Sxpr
listP' = asPair `pmap` delimP (oneOrMore wsP) sxprP
  where
    asPair (s:ss) = S.Pair s (asPair ss)
    asPair [] = S.Nil

listP :: Parser S.Sxpr
listP = parensP listP'

cellP :: Parser S.Sxpr
cellP = parensP cellP'

atomP :: Parser S.Sxpr
atomP = boolP `orElse` intP `orElse` symP `orElse` zilchP
  where
    boolP = b "#t" S.T `orElse` b "#f" S.F
    b s f = (S.Lit . const f) `pmap` strP s
    intP = (S.Lit . (S.Num . fromIntegral)) `pmap` integerP

sxprP :: Parser S.Sxpr
sxprP = lsp $ qSxprP `orElse` atomP `orElse` cellP `orElse` listP
  where
    lsp p = pmap snd $ zeroOrMore wsP `foll` p
    qSxprP = qteP `orElse` qqteP `orElse` uqteP
    qteP = q '\'' S.Qt
    qqteP = q '`' S.Qqt
    uqteP = q ',' S.Uqt
    q :: Char -> (S.Sxpr -> S.Sxpr) -> Parser S.Sxpr
    q c f = (f . snd) `pmap` (char c `foll` sxprP)
