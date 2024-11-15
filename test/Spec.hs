{-# LANGUAGE QuasiQuotes #-}

import Test.QuickCheck
import qualified Fun.Json as J
import qualified Fun.Sxpr as S

import qualified Fun.PC2 (Parser(run))
import qualified Fun.PC3 (Parser(run), Res(..))

import qualified Fun.PC1.Json
import qualified Fun.PC2.Json
import qualified Fun.PC3.Json

import qualified Fun.PC1.Sxpr
import qualified Fun.PC2.Sxpr
import qualified Fun.PC3.Sxpr

import Fun.Utils (Render(..), Rendering(..), orL)
import qualified Fun.Scheme1
import qualified Fun.Scheme2
import qualified Fun.Scheme3

import Data.Either (isRight, partitionEithers)
import Data.List (isSubsequenceOf)
import Text.RawString.QQ
import Control.Exception (try, SomeException (SomeException))

import Test.Hspec

import qualified Scheme33Interp1

newtype ST = ST (String, Maybe String) deriving Show

type ErrMatches = [String]
newtype ST2 = ST2 (String, Either ErrMatches String) deriving Show
newtype ST3 = ST3 (String, Either ErrMatches String) deriving Show

prop_roundTripJson :: J.Json -> Bool
prop_roundTripJson j = parsed == [(j, "")]
  where parsed = Fun.PC1.Json.parse s
        s      = show j

prop_roundTripJson2 :: J.Json -> Bool
prop_roundTripJson2 j = parsed == Right (j, "")
  where parsed = Fun.PC2.run Fun.PC2.Json.parse s
        s      = show j

prop_roundTripJson3 :: J.Json -> Bool
prop_roundTripJson3 j = parsed == Fun.PC3.Ok j ""
  where parsed = Fun.PC3.run Fun.PC3.Json.parse s
        s      = show j

prop_roundTripJson' :: Rendering J.Json -> Bool
prop_roundTripJson' (Rendering j s) =
  (fst <$> Fun.PC1.Json.parse s) == [j]

prop_roundTripJson2' :: Rendering J.Json -> Bool
prop_roundTripJson2' (Rendering j s) =
  (fst <$> Fun.PC2.run Fun.PC2.Json.parse s) == Right j

prop_roundTripJson3' :: Rendering J.Json -> Bool
prop_roundTripJson3' (Rendering j s) =
  case Fun.PC3.run Fun.PC3.Json.parse s of
    Fun.PC3.Ok got _ | got == j -> True
    _ -> False

prop_roundTripSxpr :: S.Sxpr -> Bool
prop_roundTripSxpr e = parsed == [(e, "")]
  where parsed = Fun.PC1.Sxpr.sxprP s
        s      = render e

prop_roundTripSxpr' :: Rendering S.Sxpr -> Bool
prop_roundTripSxpr' (Rendering x s) =
  (fst <$> Fun.PC1.Sxpr.sxprP s) == [x]

prop_roundTripSxpr2 :: S.Sxpr -> Bool
prop_roundTripSxpr2 e = parsed == Right (e, "")
  where parsed = Fun.PC2.run Fun.PC2.Sxpr.sxprP s
        s      = render e

prop_roundTripSxpr2' :: Rendering S.Sxpr -> Bool
prop_roundTripSxpr2' (Rendering x s) =
  case Fun.PC2.run Fun.PC2.Sxpr.sxprP s of
    Right (got, _) | got == x -> True
    _ -> False

prop_roundTripSxpr3 :: S.Sxpr -> Bool
prop_roundTripSxpr3 e = parsed == Fun.PC3.Ok e ""
  where parsed = Fun.PC3.run Fun.PC3.Sxpr.sxprP s
        s      = render e

prop_roundTripSxpr3' :: Rendering S.Sxpr -> Bool
prop_roundTripSxpr3' (Rendering x s) =
  case Fun.PC3.run Fun.PC3.Sxpr.sxprP s of
    Fun.PC3.Ok got _ | got == x -> True
    _ -> False

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

prop_SchemeEval3 :: ST3 -> IO Bool
prop_SchemeEval3 st = case st of
  ST3 (inp, Right out) ->
    do
      got <- parse inp >>= eval
      expected <- parse out
      return $ got == expected
  ST3 (inp, Left errMatches) ->
    do
      Left (SomeException msg) <- try (parse inp >>= eval)
      return $ matches $ show msg
    where
      matches err = all (`isSubsequenceOf` err) errMatches
  where
    eval s = snd <$> Fun.Scheme3.eval Fun.Scheme3.initEnv s
    parse s = either fail (return . Fun.Scheme3.fromSxpr) $ parse' s
    parse' s = fst <$> Fun.PC2.run Fun.PC2.Sxpr.sxprP s


main :: IO ()
main = do
  allChecks
  hspec Scheme33Interp1.interpSpec

allChecks :: IO ()
allChecks = do
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
        .&&. prop_roundTripJson3
        .&&. prop_roundTripJson'
        .&&. prop_roundTripJson2'
        .&&. prop_roundTripJson3'
      prop_Sxpr = prop_roundTripSxpr
        .&&. prop_roundTripSxpr2
        .&&. prop_roundTripSxpr3
        .&&. prop_roundTripSxpr'
        .&&. prop_roundTripSxpr2'
        .&&. prop_roundTripSxpr3'
      prop_Scheme = prop_SchemeEval
        .&&. prop_SchemeEval2
        .&&. (ioProperty . prop_SchemeEval3)

instance Arbitrary ST3 where
  arbitrary =
    oneof
      [ toST3 <$> (arbitrary :: Gen ST2)
      , test [r|(apply + '(2 3 5))|] "10"
      , test [r|
          (let ((args '(3 4 5)))
              (apply + args))
          |] "12"
      , test [r|
          (let ((args (cons 3 (cons 4 (cons 5 '())))))
              (apply + args))
          |] "12"
      , test [r|
          (let ((x 22))
            (let ((f *)
                  (args `(3 ,x)))
              (apply f args)))
          |] "66"
      , test "(eval 1)" "1"
      , test [r|(eval '(+ 1 3 13))|] "17"
      , test [r|
          (let ((f +)
                (args '(3 19)))
            (eval (cons f args)))
          |] "22"
      , test [r|
          (let ((env
                  '((a 7))))
              (eval '(* a a) env))
          |] "49"
      ]
    where
      toST3 (ST2 (inp, out)) = ST3 (inp, out)
      test inp out = return $ ST3 (inp, pure out)

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

