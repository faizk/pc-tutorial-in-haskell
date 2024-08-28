module Main (main) where

import qualified Fun.Scheme1.Interp1 as Scheme1 (repl)
import qualified Fun.Scheme1 as Scheme1 (initEnv)

import qualified Fun.Scheme2.Interp1 as Scheme2 (repl)
import qualified Fun.Scheme2 as Scheme2 (initEnv)

import qualified Fun.PC1.Sxpr as PC1 (sxprP)

import Data.Maybe (listToMaybe)


scheme2Repl :: IO ()
scheme2Repl =
  Scheme2.repl readP Scheme2.initEnv
  where
    readP = maybe (fail "parse error") (pure . fst) . listToMaybe . PC1.sxprP

scheme1Repl :: IO ()
scheme1Repl =
  Scheme1.repl readP Scheme1.initEnv
  where
    readP = maybe (fail "parse error") (pure . fst) . listToMaybe . PC1.sxprP

main :: IO ()
main = scheme2Repl
