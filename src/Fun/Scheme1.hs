module Fun.Scheme1
    ( eval
    , Value(..)
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
  = Cons| Car | Cdr
  deriving (Eq, Show)

data Callable
  = Lambda [String] S.Sxpr Env
  | BuiltIn BuiltIn
  deriving (Eq, Show)

data Value
  = Sxpr S.Sxpr
  | Callable Callable
  deriving (Eq, Show)

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
    argsEnv <- check fArgs args
    eval (updateEnv argsEnv env) body
  where
    check = undefined
apply _ _ = undefined


eval :: Env -> Sxpr -> Either Err Value
eval env sxpr =
  case sxpr of
    S.Qt _ -> Right $ Sxpr sxpr
    S.Nil -> Right $ Sxpr S.Nil
    S.Lit _ -> Right $ Sxpr sxpr
    S.Sym sym -> case lookup sym env of
      Just v -> Right v
      Nothing -> Left $ "undefined symbol: " ++ sym
    S.Sym "lambda" :~ args :~ body :~ S.Nil ->
      do
        argList <- toArgList args
        return $ Callable $ Lambda argList body env
    S.Sym "let" :~ bindings :~ body :~ S.Nil ->
      do
        bindings' <- rawBindings bindings
        updEnv <- evalBindings env bindings'
        eval (updateEnv updEnv env) body
    fxpr :~ argsXpr ->
      do
        argXprList <- maybeList argsXpr `orL` syntaxErr "invalid argument" argsXpr
        argVals <- mapM (eval env) argXprList
        fVal <- eval env fxpr >>= ensureCallable
        apply fVal argVals
    w ->
      Left $ "TODO: " ++ show w

syntaxErr :: String -> Sxpr -> String
syntaxErr msg context = "syntax error: " ++ msg ++ " in: " ++ show context
