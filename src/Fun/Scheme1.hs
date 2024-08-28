module Fun.Scheme1
    ( eval
    , initEnv
    , Value(..)
    , fromSxpr
    , Env
    ) where

import Control.Applicative

import qualified Fun.Sxpr as S
import Fun.Sxpr (Sxpr((:~)), maybeList)
import Fun.Utils

type Binding = (String, Value)
type Env = [Binding]

type Err = String
type Res a = Either Err a

data BuiltIn
  = Cons | Car | Cdr
  | Prod | Sum | Div | Minus | Eq | Lt | Lte | Gt | Gte
  | IsEmpty
  deriving (Eq, Show)

data Callable
  = Lambda [String] S.Sxpr Env
  | BuiltIn BuiltIn
  deriving (Eq, Show)

data Value
  = Nil
  | Lit S.Val
  | Sym String
  | Pair Value Value
  | Qt Value
  | Qqt Value
  | Uqt Value
  | Callable Callable
  deriving (Eq, Show)

instance Render Value where
  render e = case e of
    Nil -> "()"
    Lit v -> render v
    Sym s -> s
    Qt s -> '\'' : render s
    Qqt s -> '`' : render s
    Uqt s -> ',' : render s
    l `Pair` r -> "(" ++ inside ++ ")"
      where
        inside = case asList e of
          Just list -> unwords $ map render list
          Nothing -> render l ++ " . " ++ render r
        asList (h `Pair` t) = (h :) <$> asList t
        asList  Nil = pure []
        asList  _ = Nothing
    Callable proc ->
      "#<proc: " ++ show proc ++ ">"

fromSxpr :: Sxpr -> Value
fromSxpr e = case e of
  S.Nil -> Nil
  S.Lit v -> Lit v
  S.Sym s -> Sym s
  a :~ b -> fromSxpr a `Pair` fromSxpr b
  S.Qt x -> Qt $ fromSxpr x
  S.Qqt x -> Qqt $ fromSxpr x
  S.Uqt x -> Uqt $ fromSxpr x

rawBindings :: S.Sxpr -> Res [(String, S.Sxpr)]
rawBindings sxpr = do
  bindings <- maybeList sxpr `orL` syntaxErr "bad bindings" sxpr
  mapM kvPair bindings

kvPair :: S.Sxpr -> Res (String, S.Sxpr)
kvPair ((S.Sym k) :~ v :~ S.Nil) = Right (k, v)
kvPair sxpr = Left $ syntaxErr "expected name-value pair" sxpr

evalBindings :: Env -> [(String, Sxpr)] -> Res Env
evalBindings env = mapM f
  where f (binding, expr) = (,) binding <$> eval env expr

evalBindingsRec :: Env -> [(String, Sxpr)] -> Res Env
evalBindingsRec env = foldl f (Right env)
  where f envSoFar (binding, expr) =
          flip (:) env . (,) binding <$> (envSoFar >>= flip eval expr)

evalLet :: Env -> (Env -> [(String, Sxpr)] -> Res Env) -> Sxpr -> Sxpr -> Res Value
evalLet env evalF bindings body = do
  bindings' <- rawBindings bindings
  updEnv <- evalF env bindings'
  eval (updateEnv updEnv env) body

updateEnv :: Env -> Env -> Env
updateEnv new old = new ++ old -- TODO: slow

toArgList :: Sxpr -> Res [String]
toArgList e = do
  l <- maybeList e `orL` syntaxErr "not an argument list" e
  mapM check l
    where
      check (S.Sym a) = Right a
      check x = Left $ syntaxErr "Not a valid symbol: " x

ensureCallable :: Value -> Res Callable
ensureCallable (Callable c) = Right c
ensureCallable v = Left $ "uncallable value: " ++ show v

apply :: Callable -> [Value] -> Res Value
apply (Lambda fArgs body env) args =
  do
    argsEnv <- check
    eval (updateEnv argsEnv env) body
  where
    nFArgs = length fArgs
    nArgs = length args
    check = case nFArgs `compare` nArgs of
      LT -> Left $ "too many arguments:" ++ expectedVsGot
      GT -> Left $ "too few arguments:" ++ expectedVsGot
      EQ -> Right $ fArgs `zip` args
    expectedVsGot = "got " ++ show nArgs ++ ", expected " ++ show nFArgs
apply (BuiltIn op) args =
  case (op, args) of
    (Cons, [a, b]) -> Right $ a `Pair` b
    (Cons, _) -> errNArgs 2
    (Car, [h `Pair` _]) -> Right h
    (Car, [a]) -> errType "cons" (show a)
    (Car, _:_) -> errNArgs 1
    (Car, []) -> errNArgs 1
    (Cdr, [_ `Pair` t]) -> Right t
    (Cdr, [a]) -> errType "cons" (show a)
    (Cdr, _:_) -> errNArgs 1
    (Cdr, []) -> errNArgs 1

    (Prod, _) -> Lit . S.Num . product <$> mapM asNumber args
    (Sum, _) -> Lit . S.Num . sum <$> mapM asNumber args
    (Div, [a, b]) -> Lit . S.Num <$> bin a div b
    (Div, _) -> errNArgs 2
    (Minus, [a, b]) -> Lit . S.Num <$> bin a (-) b
    (Minus, _) -> errNArgs 2

    (Eq, [l,r]) -> toBool <$> bin l (==) r
    (Eq, _) -> errNArgs 2
    (Gt, [l, r]) -> toBool <$> bin l (>) r
    (Gt, _) -> errNArgs 2
    (Gte, [l, r]) -> toBool <$> bin l (>=) r
    (Gte, _) -> errNArgs 2
    (Lt, [l, r]) -> toBool <$> bin l (<) r
    (Lt, _) -> errNArgs 2
    (Lte, [l, r]) -> toBool <$> bin l (<=) r
    (Lte, _) -> errNArgs 2

    (IsEmpty, [x]) -> Right $ toBool $ x == Nil
    (IsEmpty, _) -> errNArgs 1

  where
    errType expected got = Left $ "wrong type for operator " ++ show op
      ++ ": expected " ++ expected ++ ", got " ++ got
    errNArgs :: Int -> Res a
    errNArgs n = Left $ "wrong number of args to " ++ show op ++ ": expected "
      ++ show n ++ ", got " ++ show (length args)
    asNumber (Lit (S.Num n)) = Right n
    asNumber v = Left $ "type error: not a number: " ++ show v
    toBool b = Lit $ if b then S.T else S.F
    bin a f b = f <$> asNumber a <*> asNumber b

eval :: Env -> Sxpr -> Either Err Value
eval env sxpr =
  case sxpr of
    S.Qt x -> Right $ fromSxpr x
    S.Nil -> Right Nil
    S.Lit v -> Right $ Lit v
    S.Sym sym -> case lookup sym env of
      Just v -> Right v
      Nothing -> Left $ "undefined symbol: " ++ sym
    S.Sym "lambda" :~ args :~ body :~ S.Nil ->
      do
        argList <- toArgList args
        return $ Callable $ Lambda argList body env
    S.Sym "let" :~ bindings :~ body :~ S.Nil ->
      evalLet env evalBindings bindings body
    S.Sym "let*" :~ bindings :~ body :~ S.Nil ->
      evalLet env evalBindingsRec bindings body
    S.Sym "if" :~ condE :~ thenE :~ elseE :~ S.Nil ->
      do
        cond <- eval env condE >>= asBool
        if cond then eval env thenE else eval env elseE
      where
        asBool (Lit S.T) = Right True
        asBool (Lit S.F) = Right False
        asBool bad = Left $ "type error: not a boolean: " ++ show bad

    fxpr :~ argsXpr ->
      do
        argXprList <- maybeList argsXpr `orL` syntaxErr "invalid argument" argsXpr
        argVals <- mapM (eval env) argXprList
        fVal <- eval env fxpr >>= ensureCallable
        apply fVal argVals
    w ->
      Left $ "TODO: " ++ show w

initEnv :: Env
initEnv =
  builtIns
  where
    builtIns = [ (s, Callable $ BuiltIn b) | (s, b) <- builtIns' ]
    builtIns' =
      [ ("cons", Cons)
      , ("car", Car)
      , ("cdr", Cdr)
      , ("*", Prod)
      , ("+", Sum)
      , ("/", Div)
      , ("-", Minus)
      , ("=", Eq)
      , ("<", Lt)
      , (">", Gt)
      , ("<=", Lte)
      , (">=", Gte)
      , ("empty?", IsEmpty)
      ]

syntaxErr :: String -> Sxpr -> String
syntaxErr msg context = "syntax error: " ++ msg ++ " in: " ++ show context
