{-# LANGUAGE LambdaCase #-}

module Fun.Scheme3.Interp1
    ( repl
    ) where

import Fun.Scheme3 as S3 (Env, Mem, Value(), eval, Loc(..), fromSxpr)
import Fun.Sxpr as S (Sxpr(..))
import Fun.Utils

import Control.Applicative
import Control.Monad (foldM, when)
import Control.Exception (try, catch, SomeException)
import Data.Char (isSpace)
import Data.List (intercalate)
import System.IO (hFlush, stdout, stderr, hPrint, stdin, hIsTerminalDevice, hPutStrLn)
import System.IO.Error (isEOFError)


interp :: (Env, Mem) -> Sxpr -> IO ((Env, Mem), Maybe Value)
interp (env, mem) sxpr =
  case sxpr of
    Sym "define" :~ (Sym name) :~ valSxpr :~ Nil ->
      do
        let newLoc = Loc $ length mem
            newEnv = (name, newLoc) : env
        (_, v) <- S3.eval (newEnv, mem) (fromSxpr valSxpr)
        let newMem = (newLoc, v) : mem
        return ((newEnv, newMem), Nothing)
    Sym "define" :~ (Sym fName :~ fArgsXpr) :~ fBodyExpr ->
      interp (env, mem) defn
        where
          defn = Sym "define" :~ Sym fName :~ lam :~ Nil
          lam = Sym "lambda" :~ fArgsXpr :~ fBodyExpr
    _ ->
      do
        (mem', v) <- S3.eval (env, mem) (fromSxpr sxpr)
        return ((env, mem'), Just v)

type Parser a = a -> IO [Sxpr]

repl :: Parser String -> (Env, Mem) -> IO ()
repl read' (env, mem) =
  do
    maybeSxprs <- (Just <$> readSxpr (read' . intercalate "\n")) `catch` eof
    maybe bye interpList maybeSxprs
  where
    interpList :: [Sxpr] -> IO ()
    interpList xs = try (foldM g ((env, mem), Nothing) xs)
                    >>= either err (repl read' . fst)
    g :: ((Env, Mem), Maybe Value) -> Sxpr -> IO ((Env, Mem), Maybe Value)
    g (em, _) sxpr = interp em sxpr >>= \case
      x@(_, Just v)  -> putStrLn (render v) >> return x
      x@(_, Nothing) -> return x

    bye = interactive $ hPutStrLn stderr "BYE!"
    eof e = if isEOFError e then return Nothing else ioError e
    err :: SomeException -> IO ()
    err e = hPrint stderr e >> repl read' (env, mem)

readSxpr :: Parser [String] -> IO [Sxpr]
readSxpr read' =
  prompt "> " >> loop []
  where
    prompt s = interactive (putStr s >> hFlush stdout)
    more = prompt "  "
    loop inp =
      do
        inp' <- (inp ++) . pure <$> getLine'
        read' inp' <|> (more >> loop inp')
    getLine' = getLine >>= \l ->
      if all isSpace l then more >> getLine' else return l

interactive :: IO () -> IO ()
interactive io = hIsTerminalDevice stdin >>= (`when` io)
