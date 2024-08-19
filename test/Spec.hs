import Test.QuickCheck
import qualified Fun.Json as J
import qualified Fun.Sxpr as S
import qualified Fun.PC1.Json
import qualified Fun.PC1.Sxpr
import Fun.Utils (Render(..), Rendering(..))

prop_roundTripJson :: J.Json -> Bool
prop_roundTripJson j = parsed == [(j, "")]
  where parsed = Fun.PC1.Json.parse s
        s      = show j

prop_roundTripSxpr :: S.Sxpr -> Bool
prop_roundTripSxpr e = parsed == [(e, "")]
  where parsed = Fun.PC1.Sxpr.sxprP s
        s      = render e

prop_roundTripJson' :: Rendering J.Json -> Bool
prop_roundTripJson' (Rendering j s) =
  (fst <$> Fun.PC1.Json.parse s) == [j]

main :: IO ()
main = do
  verboseCheckWith args  prop_roundTrips
    where
      args = Args {
        replay = Nothing,
        maxSuccess = 100,
        maxSize = 40,
        maxDiscardRatio = 1,
        maxShrinks = 10,
        chatty = True
      }
      prop_roundTrips = prop_roundTripJson
        .&&. prop_roundTripJson'
        .&&. prop_roundTripSxpr

