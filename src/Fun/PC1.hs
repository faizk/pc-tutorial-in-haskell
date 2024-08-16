module Fun.PC1
    ( Parser
    , anyChar
    , charP
    , digitP
    , oneOrMore
    , zeroOrMore
    , orElse
    , happy
    , quietlySad
    , foll
    , pmap
    , psequence
    , naturalP
    , strP
    , delimP
    ) where

import Numeric.Natural (Natural)

type Parser a = String -> [(a, String)]

anyChar :: Parser Char
anyChar "" = []
anyChar (c:cs) = [(c, cs)]

charP :: Char -> Parser Char
charP c (a:rest) | a == c = [(a, rest)]
charP _ _ = []

digitP :: Parser Natural
digitP ('0':cs) = [(0, cs)]
digitP ('1':cs) = [(1, cs)]
digitP ('2':cs) = [(2, cs)]
digitP ('3':cs) = [(3, cs)]
digitP ('4':cs) = [(4, cs)]
digitP ('5':cs) = [(5, cs)]
digitP ('6':cs) = [(6, cs)]
digitP ('7':cs) = [(7, cs)]
digitP ('8':cs) = [(8, cs)]
digitP ('9':cs) = [(9, cs)]
digitP _ = []


orElse :: Parser a -> Parser a -> Parser a
orElse pl pr s = case pl s of
  h:t -> h:t
  [] -> pr s

happy :: a -> Parser a
happy a s = [(a, s)]

quietlySad :: Parser a
quietlySad = const []

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p `orElse` happy []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p s = do
        (a, rest) <- p s
        (l, rest') <- zeroOrMore p rest
        return (a:l, rest')

foll :: Parser a -> Parser b -> Parser (a, b)
foll pa pb s = do
  (a, rest)  <- pa s
  (b, rest') <- pb rest
  return ((a, b), rest')

pmap :: (a -> b) -> Parser a -> Parser b
pmap f pa = map (\(a, rest) -> (f a, rest)) . pa

naturalP :: Parser Natural
naturalP = pmap digitsToNat $ oneOrMore digitP

psequence :: [Parser a] -> Parser [a]
psequence [] s = happy [] s
psequence (p:ps) s = do
  (a, rest)   <- p s
  (as, rest') <- psequence ps rest
  return (a:as, rest')

strP :: String -> Parser String
strP = psequence . map charP

delimP :: Parser s -> Parser a -> Parser [a]
delimP pSep pa = pmap (uncurry (:)) p `orElse` happy []
    where
      p = pa `foll` zeroOrMore dpa
      dpa = pmap snd $ pSep `foll` pa

--- utils
digitsToNat :: [Natural] -> Natural
digitsToNat ds = sum [d * (10^p) | (d,p) <- reverse ds `zip` [0..]]

