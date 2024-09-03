module Main (main) where

import qualified Fun.Scheme1.Interp1 as Scheme1 (repl)
import qualified Fun.Scheme1 as Scheme1 (initEnv)

import qualified Fun.Scheme2.Interp1 as Scheme2 (repl)
import qualified Fun.Scheme2 as Scheme2 (initEnv)

import qualified Fun.Scheme3.Interp1 as Scheme3 (repl)
import qualified Fun.Scheme3 as Scheme3 (initEnv)

import qualified Fun.PC1.Sxpr as PC1 (sxprP)
import qualified Fun.PC3.Sxpr as PC3 (sxprP)

import qualified Fun.PC3 as PC3 (run, Res(..), wsP, sepByP)

import Data.Maybe (listToMaybe)
import Control.Applicative

import System.IO (stderr, hPutStrLn)

warn :: String -> IO ()
warn msg = hPutStrLn stderr ("[WARN] " ++ msg)

toIO :: PC3.Res a -> IO a
toIO (PC3.Ok x []) = pure x
toIO (PC3.Ok x trailing) = warn ("trailing text ignored: " ++ trailing) >> pure x
toIO (PC3.Bad reason) = fail $ "parse error:" ++ reason

scheme31Repl :: IO ()
scheme31Repl = Scheme3.repl readP Scheme3.initEnv
  where readP = toIO . PC3.run (PC3.sepByP PC3.wsP PC3.sxprP)

scheme23Repl :: IO ()
scheme23Repl = Scheme2.repl readP Scheme2.initEnv
  where readP = toIO . PC3.run PC3.sxprP

scheme2Repl :: IO ()
scheme2Repl = Scheme2.repl readP Scheme2.initEnv
  where readP = maybe (fail "parse error") (pure . fst) . listToMaybe . PC1.sxprP

scheme1Repl :: IO ()
scheme1Repl =
  Scheme1.repl readP Scheme1.initEnv
  where
    readP = maybe (fail "parse error") (pure . fst) . listToMaybe . PC1.sxprP

main :: IO ()
main = scheme31Repl
