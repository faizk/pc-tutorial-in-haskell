{-# LANGUAGE GADTs, TupleSections, LambdaCase #-}

module Fun.Scheme3
    ( eval
    , Env
    , Mem
    , Loc(..)
    , initEnv
    , Value(..)
    , fromSxpr
    ) where

import qualified Fun.Sxpr as S
import Data.Traversable (mapAccumM)
import Data.Foldable (foldlM)
import Control.Monad ((>=>))

import Fun.Sxpr (Sxpr)
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
  = Lambda [String] Value Env
  | BuiltIn BuiltIn
  deriving (Eq, Show)

data Value
  = Nil
  | Lit S.Val
  | Sym String
  | Value :~ Value
  | Qt Value
  | Qqt Value
  | Uqt Value
  | Callable Callable
  deriving (Eq, Show)

infixr 5 :~

instance Render Value where
  render e = case e of
    Nil -> "()"
    Lit v -> render v
    Sym s -> s
    Qt s -> '\'' : render s
    Qqt s -> '`' : render s
    Uqt s -> ',' : render s
    l :~ r -> "(" ++ inside ++ ")"
      where
        inside = case asList e of
          Just list -> unwords $ map render list
          Nothing -> render l ++ " . " ++ render r
    Callable proc ->
      "#<proc: " ++ show proc ++ ">"

asList :: MonadFail m => Value -> m [Value]
asList (h :~ t) = (h :) <$> asList t
asList Nil = pure []
asList x = fail $ "not a list: " ++ show x

fromSxpr :: Sxpr -> Value
fromSxpr e = case e of
  S.Nil -> Nil
  S.Lit v -> Lit v
  S.Sym s -> Sym s
  a S.:~ b -> fromSxpr a :~ fromSxpr b
  S.Qt x -> Qt $ fromSxpr x
  S.Qqt x -> Qqt $ fromSxpr x
  S.Uqt x -> Uqt $ fromSxpr x

rawBindings :: MonadFail m => Value -> m [(String, Value)]
rawBindings = asList >=> mapM kvPair

kvPair :: MonadFail m => Value -> m (String, Value)
kvPair ((Sym k) :~ v :~ Nil) = pure (k, v)
kvPair sxpr = fail $ syntaxErr "expected name-value pair" sxpr

evalBindings :: MonadFail m => Env -> Mem -> [(String, Value)] -> m (Mem, Env)
evalBindings env = mapAccumM f
  where
    f mem (binding, sxpr) = g <$> eval (env, mem) sxpr
      where
        g = bind . uncurry alloc
        bind (mem', loc) = (mem', (binding, loc))

evalBindingsStar :: MonadFail m => Env -> Mem -> [(String, Value)] -> m (Mem, Env)
evalBindingsStar env mem = foldlM f (mem, env)
  where
    f (mem', env') (binding, sxpr) = g <$> eval (env', mem') sxpr
      where
        g = bind . uncurry alloc
        bind (m, l) = (m, (binding, l) : env')

evalBindingsRec :: MonadFail m => Env -> Mem -> [(String, Value)] -> m (Mem, Env)
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

evalLet :: MonadFail m => Env -> Mem -> (Env -> Mem -> [(String, Value)] -> m (Mem, Env)) -> Value -> Value -> m (Mem, Value)
evalLet env mem evalF bindings body = do
  bindings' <- rawBindings bindings
  (mem', env') <- evalF env mem bindings'
  eval (updateEnv env' env, mem') body

updateEnv :: Env -> Env -> Env
updateEnv new old = new ++ old -- TODO: slow

toArgList :: MonadFail m => Value -> m [String]
toArgList = asList >=> mapM check
    where
      check (Sym a) = pure a
      check x = fail $ syntaxErr "Not a valid symbol: " x

doApply :: MonadFail m => Env -> Mem -> Value -> [Value] -> m (Mem, Value)
doApply env mem fxpr argVals = do
  (mem', maybeFunVal) <- eval (env, mem) fxpr
  fVal <- ensureCallable maybeFunVal
  apply fVal argVals mem'
  where
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
      (Cons, [a, b]) -> pure $ a :~ b
      (Cons, _) -> errNArgs 2
      (Car, [h :~ _]) -> pure h
      (Car, [a]) -> errType "cons" (show a)
      (Car, _:_) -> errNArgs 1
      (Car, []) -> errNArgs 1
      (Cdr, [_ :~ t]) -> pure t
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

eval :: MonadFail m => (Env, Mem) -> Value -> m (Mem, Value)
eval (env, mem) = \case
  Qt x -> pure (mem, x)
  Qqt (h :~ t) -> both h t mem
    where
      both h' t' mem' = do
        (mem'', hv) <- evalUq h' mem'
        (mem''', tv) <- evalUq t' mem''
        return (mem''', hv :~ tv)
      evalUq (Uqt uqt) m = eval (env, m) uqt
      evalUq (h' :~ t') m = both h' t' m
      evalUq e m = pure (m, e)
  Qqt x -> pure (mem, x)
  Uqt x -> fail $ "not in qq: " ++ show x
  Nil -> pure (mem, Nil)
  Lit v -> pure (mem, Lit v)
  Sym sym -> maybe (fail $ "undefined symbol: " ++ sym) (return . (mem,)) $
    lookup sym env >>= flip lookup mem
  Sym "lambda" :~ args :~ body :~ Nil ->
    do argList <- toArgList args
       return (mem, Callable $ Lambda argList body env)
  Sym "let" :~ bindings :~ body :~ Nil ->
    evalLet env mem evalBindings bindings body
  Sym "let*" :~ bindings :~ body :~ Nil ->
    evalLet env mem evalBindingsStar bindings body
  Sym "letrec" :~ bindings :~ body :~ Nil ->
    evalLet env mem evalBindingsRec bindings body
  Sym "if" :~ condE :~ thenE :~ elseE :~ Nil ->
    do cond <- eval (env, mem) condE >>= asBool . snd
       if cond then eval (env, mem) thenE else eval (env, mem) elseE
    where
      asBool (Lit S.T) = pure True
      asBool (Lit S.F) = pure False
      asBool bad = fail $ "type error: not a boolean: " ++ show bad
  Sym "apply" :~ fxpr :~ argsXpr :~ Nil ->
    do (mem', argsVal) <- eval (env, mem) argsXpr
       argsValList <- asList argsVal
       doApply env mem' fxpr argsValList
  fxpr :~ argsXpr ->
    do argXprList <- asList argsXpr
       (mem', argValsList) <- mapAccumM g mem argXprList
       doApply env mem' fxpr argValsList
    where
      g m = eval (env, m)
  Callable callable ->
    return (mem, Callable callable)

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
initEnv = foldl f ([], []) builtIns
  where
    f (env, mem) (name, value) = (env', mem')
        where
          (mem', loc) = alloc mem value
          env' = (name, loc) : env

syntaxErr ::Show a => String -> a -> String
syntaxErr msg context = "syntax error: " ++ msg ++ " in: " ++ show context
