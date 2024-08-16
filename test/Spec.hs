import Test.QuickCheck
import qualified Fun.Json as J
import Fun.PC1.Json

prop_roundTrip :: J.Json -> Bool
prop_roundTrip j = parsed == [(j, "")]
  where parsed = parse s
        s      = show j

main :: IO ()
main = do
  quickCheck prop_roundTrip

