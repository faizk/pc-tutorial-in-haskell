{-# LANGUAGE TupleSections #-}

module Fun.Scheme1.Interp1
    ( repl
    ) where

import Fun.Scheme1 as S1 (Env, Value(), eval)
import Fun.Sxpr as S (Sxpr(..))
import Fun.Utils

import Control.Applicative
import Control.Exception (try, SomeException)
import Data.Char (isSpace)
import Data.List (intercalate)
import System.IO (hFlush, stdout, stderr, hPrint)


interp :: MonadFail m => Env -> Sxpr -> m (Either Env Value)
interp env sxpr =
  case sxpr of
    Sym "define" :~ (Sym name) :~ valSxpr :~ Nil ->
      Left . (: env) . (name, ) <$> eval' valSxpr
    _ ->
      Right <$> eval' sxpr
  where
      eval' e = either fail return $ S1.eval env e

type Parser a = a -> IO Sxpr

repl :: Parser String -> Env -> IO ()
repl read' env =
  do
    x <- readSxpr (read' . intercalate "\n")
    interp' x >>= either err (either newEnv result)
  where
    err :: SomeException -> IO ()
    err e = hPrint stderr e >> repl read' env
    result v = putStrLn (render v) >> repl read' env
    newEnv = repl read'
    interp' = try . interp env

readSxpr :: Parser [String] -> IO Sxpr
readSxpr read' =
  prompt "> " >> loop []
  where
    prompt s = putStr s >> hFlush stdout
    more = prompt "  "
    loop inp =
      do
        inp' <- (inp ++) . pure <$> getLine'
        read' inp' <|> (more >> loop inp')
    getLine' = getLine >>= \l ->
      if all isSpace l then more >> getLine' else return l
