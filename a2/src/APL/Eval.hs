module APL.Eval
  ( Val (..),
    eval,
    runEval,
    Error,
  )
where

import APL.AST (Exp (..), VName)
import Control.Monad (ap, liftM)

---------------------1.  VALUES AND ENVIRONMENTS ------------------

-- val is the result of evaluating an APL expression
data Val
  = ValInt Integer
  | ValBool Bool
  | ValFun Env VName Exp
  deriving (Eq, Show)


-- Env keeps track of which variables are in scope
type Env = [(VName, Val)]

-- The empty environment
envEmpty :: Env
envEmpty = []

-- add a variable to an environment
envExtend :: VName -> Val -> Env -> Env
envExtend v val env = (v, val) : env

-- look up a variable in the environment
envLookup :: VName -> Env -> Maybe Val
envLookup v env = lookup v env


--------------------2. ERRORS AND THE EvalM MONAD ---------------------

-- error produced during evaluation are strings
type Error = String

--Part 1
-- State stores the strings printed during evaluation
type State = [String]


--- EvalM is the monad used by the evaluator
-- it takes an environment, and either fails with an error,
-- or produces a value of type a
newtype EvalM a = EvalM (Env -> State -> Either Error (State, a))


--- Functor instance
-- Takes a pure value, and puts in within some effect/context
instance Functor EvalM where
  fmap = liftM


-- Applicative instance
-- Then function is within some context, AND the value is within some context
instance Applicative EvalM where
  pure x = EvalM $ \_env state -> Right (state, x)
  (<*>) = ap


-- Monad instance
instance Monad EvalM where
  EvalM x >>= f = EvalM $ \env state->
    case x env state of
      Left err -> Left err
      Right (state', x') ->
        let EvalM y = f x'
         in y env state'

--------------------------- 3. BASIC OPERATIONS ON THE EvalM monad --------------

-- Get the current environment
askEnv :: EvalM Env
askEnv = EvalM $ \env state -> Right (state, env)

-- Temporarily alter the environment while doing a computation
localEnv :: (Env -> Env) -> EvalM a -> EvalM a
localEnv f (EvalM m) = EvalM $ \env state ->
  m (f env) state

-- Fail the current computation with an error message.
failure :: String -> EvalM a
failure s = EvalM $ \_env _state-> Left s


-- Try the first computation
-- If the first computation fails, evaluate the second computation instead
catch :: EvalM a -> EvalM a -> EvalM a
catch (EvalM m1) (EvalM m2) = EvalM $ \env state->
  case m1 env state of
    Left _ -> m2 env state
    Right (state', x) -> Right (state', x)

----------------------------- 4. RUNNING AN EVALUATION ----------------

--  TASK 1
-- runEval :: EvalM a -> Either Error a
-- runEval (EvalM m) = m envEmpty
runEval :: EvalM a -> ([String], Either Error a)
runEval (EvalM m) =
  case m envEmpty [] of
    Left err -> ([], Left err)
    Right (state, value) -> (state, Right value)
    



-------------------------5. HELPER FUNCTIONS FOR EVALUATING EXPRESSIONS------------

evalIntBinOp :: (Integer -> Integer -> EvalM Integer) -> Exp -> Exp -> EvalM Val
evalIntBinOp f e1 e2 = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (ValInt x, ValInt y) -> ValInt <$> f x y
    (_, _) -> failure "Non-integer operand"

evalIntBinOp' :: (Integer -> Integer -> Integer) -> Exp -> Exp -> EvalM Val
evalIntBinOp' f e1 e2 =
  evalIntBinOp f' e1 e2
  where
    f' x y = pure $ f x y


--------------------- 6. THE EVALUATOR----
-- we did all of part 6 in last weeks assignment

eval :: Exp -> EvalM Val
eval (CstInt x) = pure $ ValInt x
eval (CstBool b) = pure $ ValBool b
eval (Var v) = do
  env <- askEnv
  case envLookup v env of
    Just x -> pure x
    Nothing -> failure $ "Unknown variable: " ++ v
eval (Add e1 e2) = evalIntBinOp' (+) e1 e2
eval (Sub e1 e2) = evalIntBinOp' (-) e1 e2
eval (Mul e1 e2) = evalIntBinOp' (*) e1 e2
eval (Div e1 e2) = evalIntBinOp checkedDiv e1 e2
  where
    checkedDiv _ 0 = failure "Division by zero"
    checkedDiv x y = pure $ x `div` y
eval (Pow e1 e2) = evalIntBinOp checkedPow e1 e2
  where
    checkedPow x y =
      if y < 0
        then failure "Negative exponent"
        else pure $ x ^ y
eval (Eql e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (ValInt x, ValInt y) -> pure $ ValBool $ x == y
    (ValBool x, ValBool y) -> pure $ ValBool $ x == y
    (_, _) -> failure "Invalid operands to equality"
eval (If cond e1 e2) = do
  cond' <- eval cond
  case cond' of
    ValBool True -> eval e1
    ValBool False -> eval e2
    _ -> failure "Non-boolean conditional."
eval (Let var e1 e2) = do
  v1 <- eval e1
  localEnv (envExtend var v1) $ eval e2
eval (ForLoop (loopparam, initial) (iv, bound) body) = do
  initial_v <- eval initial
  bound_v <- eval bound
  case bound_v of
    ValInt bound_int ->
      loop 0 bound_int initial_v
    _ ->
      failure "Non-integral loop bound"
  where
    loop i bound_int loop_v
      | i >= bound_int = pure loop_v
      | otherwise = do
          loop_v' <-
            localEnv (envExtend iv (ValInt i) . envExtend loopparam loop_v) $
              eval body
          loop (succ i) bound_int loop_v'
eval (Lambda var body) = do
  env <- askEnv
  pure $ ValFun env var body
eval (Apply e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  case (v1, v2) of
    (ValFun f_env var body, arg) ->
      localEnv (const $ envExtend var arg f_env) $ eval body
    (_, _) ->
      failure "Cannot apply non-function"
eval (TryCatch e1 e2) =
  eval e1 `catch` eval e2
