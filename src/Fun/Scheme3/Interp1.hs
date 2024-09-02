{-# LANGUAGE TupleSections #-}

module Fun.Scheme3.Interp1
    ( repl
    ) where

import Fun.Scheme3 as S3 (Env, Mem, Value(), eval, Loc(..), fromSxpr)
import Fun.Sxpr as S (Sxpr(..))
import Fun.Utils

import Control.Applicative
import Control.Monad ((>=>))
import Control.Exception (try, catch, SomeException)
import Data.Char (isSpace)
import Data.List (intercalate)
import System.IO (hFlush, stdout, stderr, hPrint)
import qualified Data.Bifunctor
import System.IO.Error (isEOFError)


interp :: (Env, Mem) -> Sxpr -> IO (Mem, Either Env Value)
interp (env, mem) sxpr =
  case sxpr of
    Sym "define" :~ (Sym name) :~ valSxpr :~ Nil ->
      do
        let newLoc = Loc $ length mem
            newEnv = (name, newLoc) : env
        (_, v) <- S3.eval (newEnv, mem) (fromSxpr valSxpr)
        let newMem = (newLoc, v) : mem
        return (newMem, Left newEnv)
    Sym "define" :~ (Sym fName :~ fArgsXpr) :~ fBodyExpr ->
      interp (env, mem) defn
        where
          defn = Sym "define" :~ Sym fName :~ lam :~ Nil
          lam = Sym "lambda" :~ fArgsXpr :~ fBodyExpr
    _ ->
      Data.Bifunctor.second Right <$> S3.eval (env, mem) (fromSxpr sxpr)

type Parser a = a -> IO Sxpr

repl :: Parser String -> (Env, Mem) -> IO ()
repl read' (env, mem) =
  do
    maybeSxpr <- (Just <$> readSxpr (read' . intercalate "\n")) `catch` eof
    maybe (print "BYE") (interp' >=> either err ok) maybeSxpr
  where
    eof e = if isEOFError e then return Nothing else ioError e
    err :: SomeException -> IO ()
    err e = hPrint stderr e >> repl read' (env, mem)
    ok (mem', r) = either (repl read' . (, mem')) result r
    result v = putStrLn (render v) >> repl read' (env, mem)
    interp' = try . interp (env, mem)

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
