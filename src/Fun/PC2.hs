module Fun.PC2
    ( Parser
    , charP
    , anyChar
    , digitP
    ) where

import Data.Bifunctor (first)
import Data.Char (isDigit)
import Control.Monad ((>=>))

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

charP :: (Char -> Bool) -> Parser Char
charP f = Parser p
  where p (c:cs) | f c  = Right (c, cs)
        p []            = Left "EOF"
        p (c:_)         = Left $ "unexpected char: :" ++ [c]

anyChar :: Parser Char
anyChar = Parser p where
  p (c:cs) = Right (c,cs)
  p _      = Left "EOF"

digitP :: Parser Int
digitP = charP isDigit >>= (\c -> pure $ length ['1'..c])
