{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}

module Fun.Scheme2
    ( eval
    , initEnv
    , Value(..)
    , fromSxpr
    , evalBindingsRec
    ) where

import Control.Applicative

import qualified Fun.Sxpr as S
import Fun.Sxpr (Sxpr((:~)), maybeList)
import Fun.Utils
import Data.Traversable (mapAccumM)
import Data.Foldable (foldlM, foldrM)

data Loc where
  Loc :: Int -> Loc
  deriving (Show, Eq)

type Binding = (String, Loc)
type Env = [Binding]

type Allocation = (Loc, Value)
type Mem = [Allocation]

alloc :: Mem -> Value -> (Mem, Loc)
alloc mem v = ((newLoc, v) : mem, newLoc)
  where newLoc = Loc $ length mem

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

evalBindings :: Env -> Mem -> [(String, Sxpr)] -> Res (Mem, Env)
evalBindings env = mapAccumM f
  where
    f :: Mem -> (String, Sxpr) -> Res (Mem, Binding)
    f mem (binding, sxpr) = g <$> eval (env, mem) sxpr
      where
        g = bind . uncurry alloc
        bind (mem', loc) = (mem', (binding, loc))

evalBindingsStar :: Env -> Mem -> [(String, Sxpr)] -> Res (Mem, Env)
evalBindingsStar env mem = foldlM f (mem, env)
  where
    f :: (Mem, Env) -> (String, Sxpr) -> Res (Mem, Env)
    f (mem', env') (binding, sxpr) = g <$> eval (env', mem') sxpr
      where
        g = bind . uncurry alloc
        bind (m, l) = (m, (binding, l) : env')

evalBindingsRec :: Env -> Mem -> [(String, Sxpr)] -> Res (Mem, Env)
evalBindingsRec env mem bindings =
  (, newEnv) <$> foldlM f mem (map snd bindings)
    where
      f :: Mem -> Sxpr -> Res Mem
      f mem' sxpr = do
        (mem'', v) <- eval (newEnv, mem') sxpr
        return $ fst $ alloc mem'' v

      newEnv = newAllocs ++ env
      newLocs = map (Loc . (+) base) [0..]
      newAllocs = map fst bindings `zip` newLocs
      base = length mem

evalLet :: Env -> Mem -> (Env -> Mem -> [(String, Sxpr)] -> Res (Mem, Env)) -> Sxpr -> Sxpr -> Res (Mem, Value)
evalLet env mem evalF bindings body = do
  bindings' <- rawBindings bindings
  (mem', env') <- evalF env mem bindings'
  eval (updateEnv env' env, mem') body

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

apply :: Callable -> [Value] -> Mem -> Res (Mem, Value)
apply (Lambda fArgs body env) args mem =
  do
    (mem', argsEnv) <- check
    eval (updateEnv argsEnv env, mem') body
  where
    nFArgs = length fArgs
    nArgs = length args
    check = case nFArgs `compare` nArgs of
      LT -> Left $ "too many arguments:" ++ expectedVsGot
      GT -> Left $ "too few arguments:" ++ expectedVsGot
      EQ -> Right $ foldl g (mem, []) $ fArgs `zip` args
    g (m, e) (name, val) = (m', e')
      where
        (m', loc) = alloc m val
        e' = (name, loc) : e
    expectedVsGot = "got " ++ show nArgs ++ ", expected " ++ show nFArgs
apply (BuiltIn op) args mem = (,) mem <$> res
  where
    res = case (op, args) of
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

    errType expected got = Left $ "wrong type for operator " ++ show op
      ++ ": expected " ++ expected ++ ", got " ++ got
    errNArgs :: Int -> Res a
    errNArgs n = Left $ "wrong number of args to " ++ show op ++ ": expected "
      ++ show n ++ ", got " ++ show (length args)
    asNumber (Lit (S.Num n)) = Right n
    asNumber v = Left $ "type error: not a number: " ++ show v
    toBool b = Lit $ if b then S.T else S.F
    bin a f b = f <$> asNumber a <*> asNumber b

eval :: (Env, Mem) -> Sxpr -> Either Err (Mem, Value)
eval (env, mem) sxpr =
  case sxpr of
    S.Qt x -> Right (mem, fromSxpr x)
    S.Qqt (h :~ t) -> both h t mem
      where
        both h' t' mem' = do
          (mem'', hv) <- evalUq h' mem'
          (mem''', tv) <- evalUq t' mem''
          return (mem''', hv `Pair` tv)
        evalUq (S.Uqt uqt) m = eval (env, m) uqt
        evalUq (h' :~ t') m = both h' t' m
        evalUq e m = Right (m, fromSxpr e)
    S.Qqt x -> Right (mem, fromSxpr x)
    S.Uqt x -> Left $ "not in qq: " ++ show x
    S.Nil -> Right (mem, Nil)
    S.Lit v -> Right (mem, Lit v)
    S.Sym sym -> case lookup sym env >>= flip lookup mem of
      Just v -> Right (mem, v)
      Nothing -> Left $ "undefined symbol: " ++ sym
    S.Sym "lambda" :~ args :~ body :~ S.Nil ->
      do
        argList <- toArgList args
        return (mem, Callable $ Lambda argList body env)
    S.Sym "let" :~ bindings :~ body :~ S.Nil ->
      evalLet env mem evalBindings bindings body
    S.Sym "let*" :~ bindings :~ body :~ S.Nil ->
      evalLet env mem evalBindingsStar bindings body
    S.Sym "letrec" :~ bindings :~ body :~ S.Nil ->
      evalLet env mem evalBindingsRec bindings body
    S.Sym "if" :~ condE :~ thenE :~ elseE :~ S.Nil ->
      do
        cond <- eval (env, mem) condE >>= asBool . snd
        if cond then eval (env, mem) thenE else eval (env, mem) elseE
      where
        asBool (Lit S.T) = Right True
        asBool (Lit S.F) = Right False
        asBool bad = Left $ "type error: not a boolean: " ++ show bad

    fxpr :~ argsXpr ->
      do
        argXprList <- maybeList argsXpr `orL` syntaxErr "invalid argument" argsXpr
        (mem', argVals) <- mapAccumM g mem argXprList
        (mem'', maybeFunVal) <- eval (env, mem') fxpr
        fVal <- ensureCallable maybeFunVal
        apply fVal argVals mem''
      where
        g m = eval (env, m)
    w ->
      Left $ "TODO: " ++ show w

builtIns :: [(String, Value)]
builtIns = map (\(k, v) -> (k, Callable $ BuiltIn v))
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

initEnv :: (Env, Mem)
initEnv =
    foldl f ([], []) builtIns
    where
      f (env, mem) (name, value) = (env', mem')
        where
          (mem', loc) = alloc mem value
          env' = (name, loc) : env

syntaxErr :: String -> Sxpr -> String
syntaxErr msg context = "syntax error: " ++ msg ++ " in: " ++ show context
