module Lib
    ( someFunc
    , charP
    , digitP
    , oneOrMore
    , zeroOrMore
    , orElse
    , happy
    , quietlySad
    , foll
    , Parser
    ) where

import Numeric.Natural (Natural)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type Parser a = String -> [(a, String)]

charP :: Char -> Parser Char
charP c = f
  where f (a:rest) | a == c = [(a, rest)]
        f _  = []

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
