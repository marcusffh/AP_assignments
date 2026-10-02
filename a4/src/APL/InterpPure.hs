module APL.InterpPure (runEval) where

import APL.Monad
import Test.Tasty.Providers (IsTest(run))

data Stop = Err Error | Brk Val -- part 4 error vs break

runEval :: EvalM a -> ([String], Either Error a)
runEval evalm = -- part 4 below
  case runEval' envEmpty stateInitial evalm of  -- part 4
    (ps, Left (Err e)) -> (ps, Left e) 
    (ps, Left (Brk _)) -> (ps, Left "Break outside loop") 
    (ps, Right x) -> (ps, Right x) -- part 4 above
  where
    runEval' :: Env -> State -> EvalM a -> ([String], Either Stop a) -- part 4
    runEval' _ _ (Pure x) = ([], pure x)
    runEval' r s (Free (ReadOp k)) = runEval' r s $ k r
    runEval' r s (Free (PrintOp p m)) =
      let (ps, res) = runEval' r s m
       in (p : ps, res)
    runEval' _ _ (Free (ErrorOp e)) = ([], Left (Err e)) -- part 4
    runEval' r s (Free (TryCatchOp e1 e2 k)) = -- part 1
      case runEval' r s e1 of
        (ps, Right v) -> 
          let (ps', res) = runEval' r s (k v)
          in (ps ++ ps', res)
        (ps, Left (Err _)) -> -- part 4: only errors are caught
          let (ps', res) = runEval' r s (e2 >>= k)
           in (ps ++ ps', res)
        (ps, Left (Brk v)) -> (ps, Left (Brk v)) -- part 4: break passes through

    runEval' r s (Free (KvGetOp key k)) =
      case lookup key s of
        Just val -> runEval' r s (k val)
        Nothing -> ([], Left (Err ("Invalid key: " ++ show key))) -- part 4

    runEval' r s (Free (KvPutOp key val k)) =
      let newState = (key, val) : filter (\(oldkey, _) -> oldkey /= key) s
      in runEval' r newState k
-- part 3 below
    runEval' r s (Free (TransactionOp m k)) = runEval' r s (m >>= k)
-- part 3 above
-- part 4 below
    runEval' _ _ (Free (BreakOp v)) = ([], Left (Brk v))
    runEval' r s (Free (LoopOp m k)) =
      case runEval' r s m of
        (ps, Right v) -> continue ps v -- body finished normally
        (ps, Left (Brk v)) -> continue ps v -- break: the loop returns v
        (ps, Left (Err e)) -> (ps, Left (Err e)) -- errors propagate
      where
        continue ps v =
          let (ps', res) = runEval' r s (k v)
           in (ps ++ ps', res)
-- part 4 above