import Test.QuickCheck

import qualified Fun.Json as J
import Stuff ()

import Fun.PC1

p1 :: J.Json -> J.Json -> Bool
p1 (J.Num a) (J.Num b) =
  (a+b) == (b+a)
p1 _ _ = True

main :: IO ()
main =
  quickCheck p1

