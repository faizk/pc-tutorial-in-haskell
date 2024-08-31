module Fun.PC2
    ( Parser
    , charP, char
    , anyChar
    , digitP
    , posIntegerP
    , integerP
    , wordP
    , sepByP
    , wsP, ws
    , surr, surr2
    ) where

import Data.Bifunctor (first)
import Data.Char (isDigit)
import Control.Monad ((>=>))
import Control.Applicative (Alternative(..))

type Res a = Either String (a, String)

newtype Parser a = Parser { run :: String -> Res a }

instance Functor Parser where
  fmap f (Parser p) = Parser (fmap (first f) . p)

instance Applicative Parser where
  pure a = Parser (\s -> Right (a, s))
  l <*> r = Parser p
    where
      p s = run l s >>= (\(lf, rest) -> run (lf <$> r) rest)

instance Monad Parser where
  p >>= f = Parser (run p >=> (\(a, rest) -> run (f a) rest))

instance MonadFail Parser where
  fail msg = Parser (\_ -> Left msg)

instance Alternative Parser

charP :: (Char -> Bool) -> Parser Char
charP f = Parser p
  where p (c:cs) | f c  = Right (c, cs)
        p []            = Left "EOF"
        p (c:_)         = Left $ "unexpected char: :" ++ [c]

char :: Char -> Parser Char
char c = charP (== c)

anyChar :: Parser Char
anyChar = Parser p where
  p (c:cs) = Right (c,cs)
  p _      = Left "EOF"

digitP :: Parser Integer
digitP = charP isDigit >>= (\c -> pure $ toInteger $ length ['1'..c])

posIntegerP :: Parser Integer
posIntegerP = fromDigits <$> some digitP
  where fromDigits ds = sum [d * (10^p) | (d,p) <- reverse ds `zip` [0..]]

integerP :: Parser Integer
integerP = posIntegerP <|> neg where
  neg = fmap negate $ char '-' >> posIntegerP

wordP :: String -> Parser String
wordP = mapM char

sepByP :: Parser s -> Parser a -> Parser [a]
sepByP ps pa = ((:) <$> pa <*> many (ps >> pa)) <|> pure []

ws :: Parser a -> Parser a
ws = surr (many wsP)

wsP :: Parser Char
wsP = foldr (<|>) (fail "not ws") wsPs
    where
      wsPs = map char " \n\t"

surr :: Parser a -> Parser b -> Parser b
surr pa pb = surr2 pa pb pa

surr2 :: Parser a -> Parser b -> Parser c -> Parser b
surr2 pa pb pc = pa >> pb <* pc
