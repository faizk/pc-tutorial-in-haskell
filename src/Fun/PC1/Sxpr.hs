module Fun.PC1.Sxpr
    ( sxprP
    ) where

import qualified Fun.Sxpr as V (Val(..))
import Fun.Sxpr (Sxpr(..))

import Fun.PC1
import Data.Char (isSpace)

zilchP :: Parser Sxpr
zilchP = pmap (const Nil) $ char '(' `foll` zeroOrMore wsP `foll` char ')'

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

cellP :: Parser Sxpr
cellP = parensP $ pmap f $ ws sxprP `foll` dot `foll` sxprP
  where
    f ((l, _), r) = l :~ r
    dot = ws $ char '.'

listP :: Parser Sxpr
listP = parensP $ asPair `pmap` delimP (oneOrMore wsP) sxprP
  where
    asPair (s:ss) = s :~ asPair ss
    asPair [] = Nil

atomP :: Parser Sxpr
atomP = boolP `orElse` intP `orElse` symP `orElse` zilchP
  where
    symP = Sym `pmap` oneOrMore symCP
    boolP = b "#t" V.T `orElse` b "#f" V.F
    b s f = (Lit . const f) `pmap` strP s
    intP = (Lit . (V.Num . fromIntegral)) `pmap` integerP

sxprP :: Parser Sxpr
sxprP = lsp $ qSxprP `orElse` atomP `orElse` cellP `orElse` listP
  where
    lsp p = pmap snd $ zeroOrMore wsP `foll` p
    qSxprP = qteP `orElse` qqteP `orElse` uqteP
    qteP = q '\'' Qt
    qqteP = q '`' Qqt
    uqteP = q ',' Uqt
    q :: Char -> (Sxpr -> Sxpr) -> Parser Sxpr
    q c f = (f . snd) `pmap` (char c `foll` sxprP)
