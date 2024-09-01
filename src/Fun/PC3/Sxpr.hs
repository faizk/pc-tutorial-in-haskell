module Fun.PC3.Sxpr
    ( sxprP
    ) where

import qualified Fun.Sxpr as V (Val(..))
import Fun.Sxpr (Sxpr(..))

import Fun.PC3
import Data.Char (isSpace)

import Control.Applicative (Alternative(..))

zilchP :: Parser Sxpr
zilchP = Nil <$ (char '(' >> many wsP <* char ')')

symCP :: Parser Char
symCP = charP f where
    f c = case c of
      ',' -> False
      '`' -> False
      '\\' -> False
      '(' -> False
      ')' -> False
      _ | isSpace c -> False
      _   -> True

parensP :: Parser a -> Parser a
parensP p = char '(' >> ws p <* char ')'

cellP :: Parser Sxpr
cellP = (:~) <$> ws sxprP <*> (ws (char '.') >> sxprP)

listP :: Parser Sxpr
listP = parensP $ asPair <$> sepByP (some wsP) sxprP
  where
    asPair (s:ss) = s :~ asPair ss
    asPair [] = Nil

atomP :: Parser Sxpr
atomP = boolP <|> intP <|> symP <|> zilchP
  where
    symP = Sym <$> some symCP
    boolP = b "#t" V.T <|> b "#f" V.F
    b s f = Lit . const f <$> wordP s
    intP = Lit . (V.Num . fromIntegral) <$> integerP

sxprP :: Parser Sxpr
sxprP = many wsP >> (qSxprP <|> atomP <|> cellP <|> listP)
  where
    qSxprP = qteP <|> qqteP <|> uqteP
    qteP = q '\'' Qt
    qqteP = q '`' Qqt
    uqteP = q ',' Uqt
    q :: Char -> (Sxpr -> Sxpr) -> Parser Sxpr
    q c f = f <$> (char c >> sxprP)
