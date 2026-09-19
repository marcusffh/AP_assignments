module APL.Check (checkExp, Error) where

import APL.AST (Exp (..), VName)

type Error = String

newtype CheckM a = CheckM ([VName] -> Either Error a)

instance Functor CheckM where
  fmap f (CheckM m) = CheckM $ \scope ->
    case m scope of
      Left err -> Left err
      Right x  -> Right (f x)

instance Applicative CheckM where
  pure x = CheckM $ \_scope -> Right x
  CheckM mf <*> CheckM mx = CheckM $ \scope ->
    case mf scope of
      Left err -> Left err
      Right f ->
        case mx scope of
          Left err -> Left err
          Right x  -> Right (f x)

instance Monad CheckM where
  CheckM x >>= f = CheckM $ \scope ->
    case x scope of
      Left err -> Left err
      Right x' ->
        let CheckM y = f x'
         in y scope

ask :: CheckM [VName]
ask = CheckM $ \scope -> Right scope

local :: VName -> CheckM a -> CheckM a
local x (CheckM m) = CheckM $ \scope ->
    m (x : scope)

failure :: Error -> CheckM a
failure err = CheckM $ \_ -> Left err

check :: Exp -> CheckM ()
check (CstInt _) = pure ()
check (CstBool _) = pure ()

check (Add e1 e2) = do
    check e1
    check e2

check (Sub e1 e2) = do
    check e1 
    check e2

check (Mul e1 e2) = do 
    check e1 
    check e2

check (Div e1 e2) = do 
    check e1
    check e2

check (Pow e1 e2) = do
    check e1 
    check e2

check (Eql e1 e2) = do
    check e1 
    check e2

check (If e1 e2 e3) = do
    check e1
    check e2
    check e3

check (Var x) = do
    scope <- ask
    if x `elem` scope
        then pure ()
        else failure ("Variable not in scope: " ++ x)

check (Let x e1 e2) = do 
    check e1
    local x (check e2)

check (ForLoop (x, e1) (y, e2) body) = do
    check e1
    check e2
    local x (local y (check body))

check (Lambda x e) =
    local x (check e)

check (Apply e1 e2) = do
    check e1
    check e2

check (TryCatch e1 e2) = do
    check e1
    check e2

check (Print _ e) = 
    check e

check (KvPut e1 e2) = do
    check e1
    check e2

check (KvGet e) =
    check e


checkExp :: Exp -> Maybe Error
checkExp e = 
    let CheckM m = check e
    in case m [] of
        Left err -> Just err
        Right () -> Nothing 
