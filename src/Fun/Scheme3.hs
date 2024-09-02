{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}

module Fun.Scheme3
    ( eval
    , Env
    , Mem
    , Loc(..)
    , initEnv
    , Value(..)
    , fromSxpr
    , evalBindingsRec
    ) where

import qualified Fun.Sxpr as S
import Data.Traversable (mapAccumM)
import Data.Foldable (foldlM)
import Control.Monad ((>=>))

import Fun.Sxpr (Sxpr((:~)), maybeList)
import Fun.Utils

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
    Callable proc ->
      "#<proc: " ++ show proc ++ ">"

asList :: MonadFail m => Value -> m [Value]
asList (h `Pair` t) = (h :) <$> asList t
asList Nil = pure []
asList x = fail $ "not a list: " ++ show x

fromSxpr :: Sxpr -> Value
fromSxpr e = case e of
  S.Nil -> Nil
  S.Lit v -> Lit v
  S.Sym s -> Sym s
  a :~ b -> fromSxpr a `Pair` fromSxpr b
  S.Qt x -> Qt $ fromSxpr x
  S.Qqt x -> Qqt $ fromSxpr x
  S.Uqt x -> Uqt $ fromSxpr x

rawBindings :: MonadFail m => S.Sxpr -> m [(String, S.Sxpr)]
rawBindings = maybeList >=> mapM kvPair

kvPair :: MonadFail m => S.Sxpr -> m (String, S.Sxpr)
kvPair ((S.Sym k) :~ v :~ S.Nil) = pure (k, v)
kvPair sxpr = fail $ syntaxErr "expected name-value pair" sxpr

evalBindings :: MonadFail m => Env -> Mem -> [(String, Sxpr)] -> m (Mem, Env)
evalBindings env = mapAccumM f
  where
    f mem (binding, sxpr) = g <$> eval (env, mem) sxpr
      where
        g = bind . uncurry alloc
        bind (mem', loc) = (mem', (binding, loc))

evalBindingsStar :: MonadFail m => Env -> Mem -> [(String, Sxpr)] -> m (Mem, Env)
evalBindingsStar env mem = foldlM f (mem, env)
  where
    f (mem', env') (binding, sxpr) = g <$> eval (env', mem') sxpr
      where
        g = bind . uncurry alloc
        bind (m, l) = (m, (binding, l) : env')

evalBindingsRec :: MonadFail m => Env -> Mem -> [(String, Sxpr)] -> m (Mem, Env)
evalBindingsRec env mem bindings =
  (, newEnv) <$> foldlM f mem (map snd bindings)
    where
      f mem' sxpr = do
        (mem'', v) <- eval (newEnv, mem') sxpr
        return $ fst $ alloc mem'' v

      newEnv = newAllocs ++ env
      newLocs = map (Loc . (+) base) [0..]
      newAllocs = map fst bindings `zip` newLocs
      base = length mem

evalLet :: MonadFail m => Env -> Mem -> (Env -> Mem -> [(String, Sxpr)] -> m (Mem, Env)) -> Sxpr -> Sxpr -> m (Mem, Value)
evalLet env mem evalF bindings body = do
  bindings' <- rawBindings bindings
  (mem', env') <- evalF env mem bindings'
  eval (updateEnv env' env, mem') body

updateEnv :: Env -> Env -> Env
updateEnv new old = new ++ old -- TODO: slow

toArgList :: MonadFail m => Sxpr -> m [String]
toArgList =  maybeList >=> mapM check
    where
      check (S.Sym a) = pure a
      check x = fail $ syntaxErr "Not a valid symbol: " x

ensureCallable :: MonadFail m => Value -> m Callable
ensureCallable (Callable c) = pure c
ensureCallable v = fail $ "uncallable value: " ++ show v

apply :: MonadFail m => Callable -> [Value] -> Mem -> m (Mem, Value)
apply (Lambda fArgs body env) args mem =
  do
    (mem', argsEnv) <- check
    eval (updateEnv argsEnv env, mem') body
  where
    nFArgs = length fArgs
    nArgs = length args
    check = case nFArgs `compare` nArgs of
      LT -> fail $ "too many arguments:" ++ expectedVsGot
      GT -> fail $ "too few arguments:" ++ expectedVsGot
      EQ -> pure $ foldl g (mem, []) $ fArgs `zip` args
    g (m, e) (name, val) = (m', e')
      where
        (m', loc) = alloc m val
        e' = (name, loc) : e
    expectedVsGot = "got " ++ show nArgs ++ ", expected " ++ show nFArgs
apply (BuiltIn op) args mem = (,) mem <$> res
  where
    res = case (op, args) of
      (Cons, [a, b]) -> pure $ a `Pair` b
      (Cons, _) -> errNArgs 2
      (Car, [h `Pair` _]) -> pure h
      (Car, [a]) -> errType "cons" (show a)
      (Car, _:_) -> errNArgs 1
      (Car, []) -> errNArgs 1
      (Cdr, [_ `Pair` t]) -> pure t
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

      (IsEmpty, [x]) -> pure $ toBool $ x == Nil
      (IsEmpty, _) -> errNArgs 1

    errType expected got = fail $ "wrong type for operator " ++ show op
      ++ ": expected " ++ expected ++ ", got " ++ got
    -- errNArgs :: Int -> m a
    errNArgs n = fail $ "wrong number of args to " ++ show op ++ ": expected "
      ++ show n ++ ", got " ++ show (length args)
    asNumber (Lit (S.Num n)) = return n
    asNumber v = fail $ "type error: not a number: " ++ show v
    toBool b = Lit $ if b then S.T else S.F
    bin a f b = f <$> asNumber a <*> asNumber b

eval :: MonadFail m => (Env, Mem) -> Sxpr -> m (Mem, Value)
eval (env, mem) sxpr =
  case sxpr of
    S.Qt x -> pure (mem, fromSxpr x)
    S.Qqt (h :~ t) -> both h t mem
      where
        both h' t' mem' = do
          (mem'', hv) <- evalUq h' mem'
          (mem''', tv) <- evalUq t' mem''
          return (mem''', hv `Pair` tv)
        evalUq (S.Uqt uqt) m = eval (env, m) uqt
        evalUq (h' :~ t') m = both h' t' m
        evalUq e m = pure (m, fromSxpr e)
    S.Qqt x -> pure (mem, fromSxpr x)
    S.Uqt x -> fail $ "not in qq: " ++ show x
    S.Nil -> pure (mem, Nil)
    S.Lit v -> pure (mem, Lit v)
    S.Sym sym -> case lookup sym env >>= flip lookup mem of
      Just v -> pure (mem, v)
      Nothing -> fail $ "undefined symbol: " ++ sym
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
        asBool (Lit S.T) = pure True
        asBool (Lit S.F) = pure False
        asBool bad = fail $ "type error: not a boolean: " ++ show bad
    S.Sym "apply" :~ fxpr :~ argsXpr :~ S.Nil ->
      do
        (mem', argsVal) <- eval (env, mem) argsXpr
        argsValList <- asList argsVal
        (mem'', maybeFunVal) <- eval (env, mem') fxpr
        fVal <- ensureCallable maybeFunVal
        apply fVal argsValList mem''
    fxpr :~ argsXpr ->
      do
        argXprList <- maybeList argsXpr
        (mem', argVals) <- mapAccumM g mem argXprList
        (mem'', maybeFunVal) <- eval (env, mem') fxpr
        fVal <- ensureCallable maybeFunVal
        apply fVal argVals mem''
      where
        g m = eval (env, m)


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
