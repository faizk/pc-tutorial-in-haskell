{-# LANGUAGE LambdaCase #-}
module Fun.PC3
    ( Parser(..)
    , Res(..)
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

import Data.Char (isDigit)
import Control.Applicative (Alternative(..))

data Res a = Bad String | Ok a String deriving (Eq, Show)

instance Functor Res where
  fmap _ (Bad reason) = Bad reason
  fmap f (Ok a rest) = Ok (f a) rest

newtype Parser a = Parser { run :: String -> Res a }

instance Functor Parser where
  fmap f p = Parser (fmap f . run p)

instance Applicative Parser where
  pure a = Parser (Ok a)
  l <*> r = Parser p where
      p s = case run l s of
        Bad reason -> Bad reason
        Ok lf rest -> run (lf <$> r) rest

instance Monad Parser where
  p >>= f = Parser g where
      g s = case run p s of
        Bad reason -> Bad reason
        Ok a rest  -> run (f a) rest

instance MonadFail Parser where
  fail = Parser . const Bad

instance Alternative Parser where
  empty = fail "nothing matched"
  pa <|> pb = Parser g where
    g s = case run pa s of
      Bad _       -> run pb s
      ok@(Ok _ _) -> ok

charP :: (Char -> Bool) -> Parser Char
charP f = anyChar >>= \case
  c | f c  -> return c
  c        -> fail $ "unexpected char: " ++ [c]

char :: Char -> Parser Char
char c = charP (== c)

anyChar :: Parser Char
anyChar = Parser $ \case
  (c:cs) -> Ok c cs
  _      -> Bad "EOF"

digitP :: Parser Integer
digitP = charP isDigit >>= (\c -> pure $ toInteger $ length ['1'..c])

posIntegerP :: Parser Integer
posIntegerP = fromDigits <$> some digitP
  where fromDigits ds = sum [d * (10^p) | (d,p) <- reverse ds `zip` [(0::Int)..]]

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
    where wsPs = map char " \n\t"

surr :: Parser a -> Parser b -> Parser b
surr pa pb = surr2 pa pb pa

surr2 :: Parser a -> Parser b -> Parser c -> Parser b
surr2 pa pb pc = pa >> pb <* pc

