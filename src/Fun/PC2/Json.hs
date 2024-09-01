module Fun.PC2.Json
    ( parse
    ) where

import Fun.PC2
    ( Parser(..)
    , char
    , integerP
    , charP
    , wordP
    , sepByP
    , ws, surr2, surr
    )

import Control.Applicative (Alternative(..))

import qualified Fun.Json as J

qStringP :: Parser String
qStringP = surr (char '"') $ many (charP (/= '"'))

boolP :: Parser J.Json
boolP = p True "true" <|> p False "false"
  where p b s = J.Bool b <$ wordP s

collP :: Parser s -> Parser a -> Parser e -> Parser [a]
collP ps pa pe = surr2 (ws ps) lp (ws pe)
  where lp = sepByP (char ',') pa

arrP :: Parser J.Json
arrP = J.Arr <$> collP (char '[') parse (char ']')

objP :: Parser J.Json
objP = J.Obj <$> collP (char '{') kvPairP (char '}')
  where
    kvPairP = (,) <$> ws keyP <*> parse
    keyP = ws (fmap J.Key qStringP) <* ws (char ':')

parse :: Parser J.Json
parse = ws $ numP <|> stringP <|> boolP <|> nullP <|> arrP <|> objP
  where
    stringP = J.Str <$> qStringP
    numP =    J.Num <$> integerP
    nullP =   J.Null <$ wordP "null"
