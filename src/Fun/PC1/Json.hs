module Fun.PC1.Json 
    (
    ) where

import Fun.PC1
    ( Parser
    , pmap
    , naturalP
    )

import qualified Fun.Json as J


numP :: Parser J.Json
numP = pmap J.Num $ pmap toInteger naturalP 

parse :: Parser J.Json
parse = numP


