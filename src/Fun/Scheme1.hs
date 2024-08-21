module Fun.Scheme1
    ( eval
    , Value(..)
    ) where

import Control.Applicative

import Fun.Sxpr
import Fun.Utils

type Binding = (String, Value)
type Env = [Binding]

type Err = String
type Res a = Either Err a

data BuiltIn
  = Cons| Car | Cdr
  deriving (Eq, Show)

data Callable
  = Lambda [String] Sxpr Env
  | BuiltIn BuiltIn
  deriving (Eq, Show)

data Value
  = Sxpr Sxpr
  | Callable Callable
  deriving (Eq, Show)

rawBindings :: Sxpr -> Res [(String, Sxpr)]
rawBindings sxpr = do
  bindings <- maybeList sxpr `orL` syntaxErr "bad bindings" sxpr
  mapM kvPair bindings

kvPair :: Sxpr -> Res (String, Sxpr)
kvPair ((Sym k) :~ v :~ Nil) = Right (k, v)
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
      check (Sym a) = Right a
      check x = Left $ syntaxErr "Not a valid symbol: " x

eval :: Env -> Sxpr -> Either Err Value
eval env sxpr =
  case sxpr of
    Qt _ -> Right $ Sxpr sxpr
    Nil -> Right $ Sxpr Nil
    Lit _ -> Right $ Sxpr sxpr
    Sym sym -> case lookup sym env of
      Just v -> Right v
      Nothing -> Left $ "undefined symbol: " ++ sym
    Sym "lambda" :~ args :~ body :~ Nil ->
      do
        argList <- toArgList args
        return $ Callable $ Lambda argList body env
    Sym "let" :~ bindings :~ body :~ Nil ->
      do
        bindings' <- rawBindings bindings
        updEnv <- evalBindings env bindings'
        eval (updateEnv updEnv env) body
    w ->
      Left $ "TODO: " ++ show w

syntaxErr :: String -> Sxpr -> String
syntaxErr msg context = "syntax error: " ++ msg ++ " in: " ++ show context
