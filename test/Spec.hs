{-# LANGUAGE QuasiQuotes #-}

import Test.QuickCheck
import qualified Fun.Json as J
import qualified Fun.Sxpr as S
import qualified Fun.PC1.Json
import Fun.PC2 (Parser(run))
import qualified Fun.PC2.Json
import qualified Fun.PC1.Sxpr
import Fun.Utils (Render(..), Rendering(..), orL)
import qualified Fun.Scheme1
import qualified Fun.Scheme2
import Data.Either (isRight, partitionEithers)

import Text.RawString.QQ
import Data.List (isSubsequenceOf)

newtype ST = ST (String, Maybe String) deriving Show

type ErrMatches = [String]
newtype ST2 = ST2 (String, Either ErrMatches String) deriving Show

prop_roundTripJson :: J.Json -> Bool
prop_roundTripJson j = parsed == [(j, "")]
  where parsed = Fun.PC1.Json.parse s
        s      = show j

prop_roundTripJson2 :: J.Json -> Bool
prop_roundTripJson2 j = parsed == Right (j, "")
  where parsed = run Fun.PC2.Json.parse s
        s      = show j

prop_roundTripSxpr :: S.Sxpr -> Bool
prop_roundTripSxpr e = parsed == [(e, "")]
  where parsed = Fun.PC1.Sxpr.sxprP s
        s      = render e

prop_roundTripJson' :: Rendering J.Json -> Bool
prop_roundTripJson' (Rendering j s) =
  (fst <$> Fun.PC1.Json.parse s) == [j]

prop_roundTripJson2' :: Rendering J.Json -> Bool
prop_roundTripJson2' (Rendering j s) =
  (fst <$> run Fun.PC2.Json.parse s) == Right j

prop_roundTripSxpr' :: Rendering S.Sxpr -> Bool
prop_roundTripSxpr' (Rendering x s) =
  (fst <$> Fun.PC1.Sxpr.sxprP s) == [x]

prop_SchemeEval :: ST -> Bool
prop_SchemeEval st = case st of
  ST (inp, Just out) ->
    (eval <$> parse inp) == (Right . Fun.Scheme1.fromSxpr <$> parse out)
  ST (inp, Nothing) ->
    not (any (isRight . eval) (parse inp))
  where
    eval = Fun.Scheme1.eval Fun.Scheme1.initEnv
    parse s = fst <$> Fun.PC1.Sxpr.sxprP s

prop_SchemeEval2 :: ST2 -> Bool
prop_SchemeEval2 st = case st of
  ST2 (inp, Right out) ->
    (eval <$> parse inp) == (Right . Fun.Scheme2.fromSxpr <$> parse out)
  ST2 (inp, Left errMatches) ->
    null successes && all matches failures
      where
        matches err = all (`isSubsequenceOf` err) errMatches
        (failures, successes) = partitionEithers $ eval <$> parse inp
  where
    eval s = snd <$> Fun.Scheme2.eval Fun.Scheme2.initEnv s
    parse s = fst <$> Fun.PC1.Sxpr.sxprP s

main :: IO ()
main = do
  verboseCheckWith args (prop_roundTrips .&&. prop_Scheme)
    where
      args = Args {
        replay = Nothing,
        maxSuccess = 500,
        maxSize = 40,
        maxDiscardRatio = 1,
        maxShrinks = 10,
        chatty = True
      }
      prop_roundTrips = prop_Json .&&. prop_Sxpr
      prop_Json = prop_roundTripJson
        .&&. prop_roundTripJson2
        .&&. prop_roundTripJson'
        .&&. prop_roundTripJson2'
      prop_Sxpr = prop_roundTripSxpr
        .&&. prop_roundTripSxpr'
      prop_Scheme = prop_SchemeEval
        .&&. prop_SchemeEval2

instance Arbitrary ST2 where
  arbitrary =
    oneof
      [ toST2 <$> (arbitrary :: Gen ST)
      , return $ ST2 ("(let ((x 23)) y)", Left ["undefined", "symbol: y"])
      , test [r|
         (letrec ((len (lambda (l)
                        (if (empty? l) 0 (+ 1 (len (cdr l))))))
                   (l2 (cons 'a (cons 'b '())))
                   (l7 '(1 2 3 4 5 6 7))
                   (l0 '()))
             (cons (len l2) (cons (len l7) (cons (len l0) '()))))
          |] "(2 7 0)"
      , test [r|
          (letrec ((map (lambda (f l)
                          (if (empty? l)
                            l
                            (cons (f (car l))
                                  (map f (cdr l)))))))
            (map (lambda (x) (+ x x))
                 '(1 2 3)))
          |] "(2 4 6)"

      , test [r|
          (let ((mk-adder (lambda (x)
                           (lambda (y) (+ x y)))))
            (let ((add7 (mk-adder 7)))
                (add7 24)))
          |] "31"

      , test "`1" "1"
      , test [r|`(+ 1 2)|] "(+ 1 2)"
      , test [r|`(+ 1 ,(+ 1 2))|] "(+ 1 3)"
      , test [r|
          `(1 ,(cons 2 `(3 ,(+ 2 2))))
          |] "(1 (2 3 4))"
      , return $ ST2 (",x", Left ["not in qq"])
      ]
    where
      toST2 (ST (inp, out)) = ST2 (inp, out `orL` [])
      test inp out = return $ ST2 (inp, pure out)


instance Arbitrary ST where
  arbitrary = oneof
    [ (\s -> ST (show s, pure $ show s)) <$> (arbitrary :: Gen Int)
    , return $ ST ("#f", pure "#f"), return $ ST ("#t", pure "#t")
    , return $ ST ("'()", pure "()")
    , return $ ST ("(let ((x 23)) x)", pure "23")
    , return $ ST ("(let ((x 23)) y)", Nothing)
    , return $ ST ("(let ((x 1)) (let ((x 7) (y (+ x 1))) y))", pure "2")
    , (\s -> ST ('\'':s, pure s))  <$> smallStr
    , test "((lambda (x) x) 7)" "7"
    , test "(cons 2 1)" "(2 . 1)"
    , test "(cons 2 (cons 1 '()))" "(2 1)"
    , test "((lambda (x) (cons x '())) '(2 1))" "((2 1))"
    , test [r|
        (let* ((x 2)
               (y (* x x)))
            y)
        |] "4"
    , test "(+ 4 7 9)" "20"
    , test "(- 71 8)" "63"
    , test "(* 13 7)" "91"
    , test "(/ 22 7)" "3"
    , test "(> 2 1)" "#t"
    , test "(< 2 1)" "#f"
    , test "(<= 2 2)" "#t"
    , test "(<= 2 1)" "#f"
    , test "(= 17 (+ 13 4))" "#t"
    , test "(= 2 1)" "#f"
    , test "(< 2 1)" "#f"
    , test "(< 17 71)" "#t"
    , test "(>= 4 1)" "#t"
    , test "(>= 4 5)" "#f"

    , test "(empty? '())" "#t"
    , test "(empty? '(a))" "#f"
    , test "(empty? 21)" "#f"
    , test "(empty? (cdr '(a b)))" "#f"
    , test "(empty? (cdr '(a)))" "#t"

    , test "(if (= 1 1) 'y 'n)" "y"
    , test "(if (= 2 1) 'y 'n)" "n"

    , test [r|
       (let
         ((Y
           (lambda (h)
             ((lambda (f) (f f))
              (lambda (f) (h (lambda (x) ((f f) x)))))))
          (fact
            (lambda (self)
              (lambda (n)
                (if (= n 0) 1
                  (* n (self (- n 1))))))))

          ((Y fact) 7))
      |] "5040"

      , test [r|'(+ 1 2)|] "(+ 1 2)"
    ]
    where
      smallStr = choose (1, 10) >>= (`vectorOf` (oneof $ map return ['a'..'z']))
      test inp out = return $ ST (inp, pure out)

