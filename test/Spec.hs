import Test.QuickCheck
import qualified Fun.Json as J
import qualified Fun.Sxpr as S
import qualified Fun.PC1.Json
import qualified Fun.PC1.Sxpr

prop_roundTripJson :: J.Json -> Bool
prop_roundTripJson j = parsed == [(j, "")]
  where parsed = Fun.PC1.Json.parse s
        s      = show j

prop_roundTripSxpr :: S.Sxpr -> Bool
prop_roundTripSxpr e = parsed == [(e, "")]
  where parsed = Fun.PC1.Sxpr.sxprP s
        s      = show e

main :: IO ()
main = do
  quickCheck prop_roundTrips
    where
      prop_roundTrips = prop_roundTripJson
        .&&. prop_roundTripSxpr

