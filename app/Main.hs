module Main (main) where

import qualified Fun.Scheme1.Interp1 as Scheme1 (repl)
import qualified Fun.Scheme1 as Scheme1 (initEnv)
import qualified Fun.PC1.Sxpr as PC1 (sxprP)
import Data.Maybe (listToMaybe)

scheme1Repl :: IO ()
scheme1Repl =
  Scheme1.repl readP Scheme1.initEnv
  where
    readP = maybe (fail "parse error") (pure . fst) . listToMaybe . PC1.sxprP

main :: IO ()
main = scheme1Repl
