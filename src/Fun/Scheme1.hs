module Fun.Scheme1
    ( eval
    ) where

import Control.Applicative

import Fun.Sxpr
import Fun.Utils

type Binding = (String, Sxpr)
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

rawBindings :: Sxpr -> Res [Binding]
rawBindings sxpr = do
  bindings <- maybeList sxpr `orL` "syntax error"
  mapM kvPair bindings

kvPair :: Sxpr -> Res (String, Sxpr)
kvPair ((Sym k) :~ v :~ Nil) = Right (k, v)
kvPair sxpr = Left $ "syntax error: expected name-value pair, got" ++ show sxpr

evalBindings :: Env -> Env -> Res Env
evalBindings env = mapM f
  where f (binding, expr) = (,) binding <$> eval env expr

updateEnv :: Env -> Env -> Env
updateEnv new old = new ++ old -- TODO: slow

eval :: Env -> Sxpr -> Either Err Sxpr
eval env sxpr =
  case sxpr of
    Qt _ -> Right sxpr
    Nil -> Right Nil
    Lit _ -> Right sxpr
    Sym sym -> case lookup sym env of
      Just v -> Right v
      Nothing -> Left $ "undefined symbol: " ++ sym
    Sym "let" :~ bindings :~ body :~ Nil ->
      do
        bindings' <- rawBindings bindings
        updEnv <- evalBindings env bindings'
        eval (updateEnv updEnv env) body
    w ->
      Left $ "TODO: " ++ show w

