module APL.Eval
  ( Val (..),
    Env,
    envEmpty,
    eval,
  )
where

import APL.AST (Exp (..), VName)


data Val
  = ValInt Integer
  | ValBool Bool
  | ValFun Env VName Exp
  deriving (Eq, Show)

type Env = [(VName, Val)]

envEmpty :: Env
envEmpty = []

envExtend :: VName -> Val -> Env -> Env
envExtend v val env = (v, val) : env

envLookup :: VName -> Env -> Maybe Val
envLookup v env = lookup v env

type Error = String

evalIntBinOp :: (Integer -> Integer -> Either Error Integer) -> Env -> Exp -> Exp -> Either Error Val
evalIntBinOp f env e1 e2 =
  case (eval env e1, eval env e2) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right (ValInt x), Right (ValInt y)) -> case f x y of
      Left err -> Left err
      Right z -> Right $ ValInt z
    (Right _, Right _) -> Left "Non-integer operand"

evalIntBinOp' :: (Integer -> Integer -> Integer) -> Env -> Exp -> Exp -> Either Error Val
evalIntBinOp' f env e1 e2 =
  evalIntBinOp f' env e1 e2
  where
    f' x y = Right $ f x y

eval :: Env -> Exp -> Either Error Val
eval _env (CstInt x) = Right $ ValInt x
eval _env (CstBool b) = Right $ ValBool b
eval env (Var v) = case envLookup v env of
  Just x -> Right x
  Nothing -> Left $ "Unknown variable: " ++ v
eval env (Add e1 e2) = evalIntBinOp' (+) env e1 e2
eval env (Sub e1 e2) = evalIntBinOp' (-) env e1 e2
eval env (Mul e1 e2) = evalIntBinOp' (*) env e1 e2
eval env (Div e1 e2) = evalIntBinOp checkedDiv env e1 e2
  where
    checkedDiv _ 0 = Left "Division by zero"
    checkedDiv x y = Right $ x `div` y
eval env (Pow e1 e2) = evalIntBinOp checkedPow env e1 e2
  where
    checkedPow x y =
      if y < 0
        then Left "Negative exponent"
        else Right $ x ^ y
eval env (Eql e1 e2) =
  case (eval env e1, eval env e2) of
    (Left err, _) -> Left err
    (_, Left err) -> Left err
    (Right (ValInt x), Right (ValInt y)) -> Right $ ValBool $ x == y
    (Right (ValBool x), Right (ValBool y)) -> Right $ ValBool $ x == y
    (Right _, Right _) -> Left "Invalid operands to equality"
eval env (If cond e1 e2) =
  case eval env cond of
    Left err -> Left err
    Right (ValBool True) -> eval env e1
    Right (ValBool False) -> eval env e2
    Right _ -> Left "Non-boolean conditional."
eval env (Let var e1 e2) =
  case eval env e1 of
    Left err -> Left err
    Right v -> eval (envExtend var v env) e2



-- For-loop implementation
eval env (ForLoop (p, initial) (i, bound) body) =
  -- Step 1: evaluate initial to a value v
  case eval env initial of
    Left err -> Left err
    Right v -> 
      
      -- Step 2: evaluate bound to an integer n
      case eval env bound of
        Left err -> Left err
        Right (ValInt n) ->

          -- Defining the loop 
          let loop counter pVal = 
                -- Step 5: While i < n
                if counter < n 
                then 
                  
                  --Step 3: Bind i to the current counter
                  --Step 4: Bind p to the current value
                  let env'' = envExtend p pVal (envExtend i (ValInt counter) env)
                  in case eval env'' body of
                    Left err -> Left err

                    -- Bind p to the result of body and increment i and repeat
                    Right newP -> loop (counter + 1) newP
                
                else 
                  --Step 6: Return the final value of p
                  Right pVal
          
          --Starting i at 0 and p at v
          in loop 0 v
        -- Step 2:The bound is not an integer  
        Right _ -> Left "Non-integral loop bound"


-- Lambda implementation
eval env (Lambda param body) = 
    Right (ValFun env param body) 

-- env means environment which we define as a list of variable names and their values
-- you should think of env as a dictionary of names and the values each name has

-- param means parameter and is the name, a function gives to its input

-- body is what the function is supposed to do

-- eval takes an environment and an expression, and returns either a value or an error
-- In this assignment evaluate mean to take an expression and figure out what value it represents



-- Apply implementation
eval env (Apply e1 e2) = --e1 is a function expression, e2 is an argument expression
  case eval env e1 of
    Left err -> Left err
    Right (ValFun funEnv param body) ->
      case eval env e2 of
        Left err -> Left err
        Right argVal ->
          eval (envExtend param argVal funEnv) body
    Right _ -> Left "valFun isnt a function" -- the case where valFun does not return a function

-- envExtend takes a variable name, a value, and an environment, and adds the variable name and value to the environment


-- eval env (TryCatch e1 e2) =
--  case eval env e1 of
--  Left err -> eval env e2
--  Right Val -> Right Val




