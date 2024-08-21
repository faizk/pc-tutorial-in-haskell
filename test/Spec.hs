import Test.QuickCheck
import qualified Fun.Json as J
import qualified Fun.Sxpr as S
import qualified Fun.PC1.Json
import qualified Fun.PC1.Sxpr
import Fun.Utils (Render(..), Rendering(..))
import qualified Fun.Scheme1
import Data.Either (isRight)

newtype ST = ST (String, Maybe String) deriving Show

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

prop_roundTripSxpr' :: Rendering S.Sxpr -> Bool
prop_roundTripSxpr' (Rendering x s) =
  (fst <$> Fun.PC1.Sxpr.sxprP s) == [x]

prop_SchemeEval :: ST -> Bool
prop_SchemeEval st = case st of
  ST (inp, Just out) ->
    (eval <$> parse inp) == (Right . Fun.Scheme1.Sxpr <$> parse out)
  ST (inp, Nothing) ->
    not (any (isRight . eval) (parse inp))
  where
    eval = Fun.Scheme1.eval []
    parse s = fst <$> Fun.PC1.Sxpr.sxprP s


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
        .&&. prop_roundTripSxpr'
        .&&. prop_SchemeEval

instance Arbitrary ST where
  arbitrary = oneof
    [ (\s -> ST (show s, pure $ show s)) <$> (arbitrary :: Gen Int)
    , return $ ST ("#f", pure "#f"), return $ ST ("#t", pure "#t")
    , return $ ST ("'()", pure "'()")
    , return $ ST ("(let ((x 23)) x)", pure "23")
    , return $ ST ("(let ((x 23)) y)", Nothing)
    , (\s -> ST (s, pure s)) . ('\'':) <$> smallStr
    ]
    where
      smallStr = choose (1, 10) >>= (`vectorOf` (oneof $ map return ['a'..'z']))


