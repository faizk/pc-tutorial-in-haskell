module Fun.PC1.Json
    ( parse
    ) where

import Fun.PC1
    ( Parser
    , pmap
    , naturalP
    , charP
    , strP
    , foll
    , orElse
    , zeroOrMore
    , delimP
    , quietlySad
    )

import qualified Fun.Json as J

char :: Char -> Parser Char
char c = charP (== c)

wsP :: Parser Char
wsP = foldr orElse quietlySad wsPs
  where
    wsPs = map char wsChars
    wsChars = " \n\t"

ws :: Parser a -> Parser a
ws = surr (zeroOrMore wsP)

numP :: Parser J.Json
numP = pmap J.Num $ pos `orElse` neg
  where
    pos = pmap toInteger naturalP
    neg = pmap (negate . toInteger . snd) neg'
    neg' = char '-' `foll` naturalP

surr :: Parser a -> Parser b -> Parser b
surr pa pb = pmap (snd . fst) $ pa `foll` pb `foll` pa

surr2 :: Parser a -> Parser b -> Parser c -> Parser b
surr2 pa pb pc = pmap (snd . fst) $ pa `foll` pb `foll` pc

qStringP :: Parser String
qStringP = surr (char '"') $ zeroOrMore (charP (/= '"'))

boolP :: Parser J.Json
boolP = p True "true" `orElse` p False "false"
  where
    p b s = pmap (const $ J.Bool b) $ strP s

nullP :: Parser J.Json
nullP = pmap (const J.Null) $ strP "null"

collP :: Parser s -> Parser a -> Parser e -> Parser [a]
collP ps pa pe = surr2 (ws ps) lp (ws pe)
  where
    lp = delimP (char ',') pa

arrP :: Parser J.Json
arrP = pmap J.Arr $ collP (char '[') parse (char ']')

objP :: Parser J.Json
objP = pmap J.Obj $ collP (char '{') (ws kvPairP) (char '}')
  where
    kvPairP = ws keyP `foll` parse
    keyP = pmap fst keyP'
    keyP' = ws (pmap J.Key qStringP) `foll` ws (char ':')

parse :: Parser J.Json
parse = ws $ numP
  `orElse` stringP
  `orElse` boolP `orElse` nullP
  `orElse` arrP `orElse` objP
    where
      stringP = pmap J.Str qStringP
