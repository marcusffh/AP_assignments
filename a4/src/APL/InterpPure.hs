module APL.InterpPure (runEval) where

import APL.Monad
import Test.Tasty.Providers (IsTest(run))

runEval :: EvalM a -> ([String], Either Error a)
runEval = runEval' envEmpty stateInitial
  where
    runEval' :: Env -> State -> EvalM a -> ([String], Either Error a)
    runEval' _ _ (Pure x) = ([], pure x)
    runEval' r s (Free (ReadOp k)) = runEval' r s $ k r
    runEval' r s (Free (PrintOp p m)) =
      let (ps, res) = runEval' r s m
       in (p : ps, res)
    runEval' _ _ (Free (ErrorOp e)) = ([], Left e)
    runEval' r s (Free (TryCatchOp e1 e2 k)) = -- part 1
      case runEval' r s e1 of
        (ps, Right v) -> 
          let (ps', res) = runEval' r s (k v)
          in (ps ++ ps', res)
        (ps, Left _) ->
          let (ps', res) = runEval' r s (e2 >>= k)
           in (ps ++ ps', res)
