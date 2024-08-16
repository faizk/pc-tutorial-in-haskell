module Fun.PC1.Json
    ( parse
    ) where

import Fun.PC1
    ( Parser
    , pmap
    , naturalP
    , charP
    , foll
    , orElse
    , zeroOrMore
    )

import qualified Fun.Json as J

numP :: Parser J.Json
numP = pmap J.Num $ pmap toInteger naturalP

surr :: Parser a -> Parser b -> Parser b
surr pa pb = pmap (snd . fst) $ pa `foll` pb `foll` pa

stringP :: Parser J.Json
stringP = pmap J.Str $ surr q $ zeroOrMore (charP (== '"'))
  where q = charP (=='"')

parse :: Parser J.Json
parse = numP `orElse` stringP
