module Fun.PC1.Json
    ( parse
    ) where

import Fun.PC1
    ( Parser
    , char
    , pmap
    , integerP
    , charP
    , strP
    , foll
    , orElse
    , zeroOrMore
    , delimP
    , ws, surr2, surr
    )

import qualified Fun.Json as J

qStringP :: Parser String
qStringP = surr (char '"') $ zeroOrMore (charP (/= '"'))

boolP :: Parser J.Json
boolP = p True "true" `orElse` p False "false"
  where
    p b s = pmap (const $ J.Bool b) $ strP s

collP :: Parser s -> Parser a -> Parser e -> Parser [a]
collP ps pa pe = surr2 (ws ps) lp (ws pe)
  where
    lp = delimP (char ',') pa

arrP :: Parser J.Json
arrP = pmap J.Arr $ collP (char '[') parse (char ']')

objP :: Parser J.Json
objP = pmap J.Obj $ collP (char '{') kvPairP (char '}')
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
      numP = pmap J.Num integerP
      nullP = pmap (const J.Null) $ strP "null"
