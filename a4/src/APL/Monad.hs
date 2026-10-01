module APL.Monad
  ( envEmpty,
    envExtend,
    envLookup,
    stateInitial,
    askEnv,
    modifyEffects,
    localEnv,
    evalPrint,
    catch,
    failure,
    evalKvGet,
    evalKvPut,
    transaction,
    looping,
    breakLoop,
    EvalM,
    Val (..),
    EvalOp (..),
    Free (..),
    Error,
    Env,
    State,
  )
where

import APL.AST (Exp (..), VName)
import Control.Monad (ap)

data Val
  = ValInt Integer
  | ValBool Bool
  | ValFun Env VName Exp
  deriving (Eq, Show)

type Error = String

type Env = [(VName, Val)]

envEmpty :: Env
envEmpty = []

envExtend :: VName -> Val -> Env -> Env
envExtend v val env = (v, val) : env

envLookup :: VName -> Env -> Maybe Val
envLookup v env = lookup v env

type State = [(Val, Val)]

stateInitial :: State
stateInitial = []

data Free e a
  = Pure a
  | Free (e (Free e a))

instance (Functor e) => Functor (Free e) where
  fmap f (Pure x) = Pure $ f x
  fmap f (Free g) = Free $ fmap (fmap f) g

instance (Functor e) => Applicative (Free e) where
  pure = Pure
  (<*>) = ap

instance (Functor e) => Monad (Free e) where
  Pure x >>= f = f x
  Free g >>= f = Free $ h <$> g
    where
      h x = x >>= f

data EvalOp a
  = ReadOp (Env -> a)
  | PrintOp String a
  | ErrorOp Error
  | TryCatchOp (EvalM Val) (EvalM Val) (Val -> a) -- part 1

instance Functor EvalOp where
  fmap f (ReadOp k) = ReadOp $ f . k
  fmap f (PrintOp p m) = PrintOp p $ f m
  fmap _ (ErrorOp e) = ErrorOp e
  fmap f (TryCatchOp e1 e2 k) =  TryCatchOp e1 e2 (f . k) -- part 1

type EvalM a = Free EvalOp a

askEnv :: EvalM Env
askEnv = Free $ ReadOp $ \env -> pure env

modifyEffects ::
  (Functor e, Functor h) =>
  (e (Free e a) -> h (Free e a)) ->
  Free e a ->
  Free h a
modifyEffects _ (Pure x) = Pure x
modifyEffects g (Free e) = Free $ modifyEffects g <$> g e

localEnv :: (Env -> Env) -> EvalM a -> EvalM a
localEnv f = modifyEffects g
  where
    g (ReadOp k) = ReadOp $ k . f
    g (TryCatchOp e1 e2 k) = TryCatchOp (localEnv f e1) (localEnv f e2) k -- part 1
    g op = op

evalPrint :: String -> EvalM ()
evalPrint p = Free $ PrintOp p $ pure ()

failure :: String -> EvalM a
failure = Free . ErrorOp

catch :: EvalM Val -> EvalM Val -> EvalM Val
catch e1 e2 = Free $ TryCatchOp e1 e2 pure --part 1

evalKvGet :: Val -> EvalM Val
evalKvGet = error "TODO"

evalKvPut :: Val -> Val -> EvalM ()
evalKvPut = error "TODO"

transaction :: EvalM Val -> EvalM Val
transaction = error "TODO"

-- | Enclose a computation @m@ such that if a 'breakLoop' is executed in @m@,
-- execution will return here.
looping :: EvalM Val -> EvalM Val
looping = error "TODO"

-- | Return the provided value from the most immediately enclosing 'looping'.
breakLoop :: Val -> EvalM a
breakLoop = error "TODO"
