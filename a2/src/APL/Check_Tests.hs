module APL.Check_Tests (tests) where

import APL.AST (Exp (..))
import APL.Check (checkExp)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

-- Assert that the provided expression should pass the type checker.
testPos :: Exp -> TestTree
testPos e =
  testCase (show e) $
    checkExp e @?= Nothing

-- Assert that the provided expression should fail the type checker.
testNeg :: Exp -> TestTree
testNeg e =
  testCase (show e) $
    case checkExp e of
      Nothing -> assertFailure "expected error"
      Just _ -> pure ()

tests :: TestTree
tests =
  testGroup
    "Checking"
    [
      testPos (CstInt 2),
      testPos (CstBool True),

      testNeg (Add (CstInt 1) (Var "x")),
      testNeg (Sub (Var "x") (CstInt 1)),
      testNeg (Mul (CstInt 1) (Var "x")),
      testNeg (Div (CstInt 1) (Var "x")),
      testNeg (Pow (CstInt 1) (Var "x")),
      testNeg (Eql (CstInt 1) (Var "x")),

      testNeg (If (Var "x") (CstInt 1) (CstInt 2)),
      testNeg (If (CstBool True) (Var "x") (CstInt 2)),
      testNeg (If (CstBool True) (CstInt 1) (Var "x")),

      testNeg (Var "x"),                                                            -- nothing in scope

      testPos (Let "x" (CstInt 1) (Var "x")),                                       -- x in scope in e2
      testNeg (Let "x" (Var "x") (CstInt 1)),                                       -- x not in scope in e1
      testNeg (Add (Let "x" (CstInt 1) (Var "x")) (Var "x")),                       -- x ends with the Let

      testPos (ForLoop ("p", CstInt 0) ("i", CstInt 3) (Add (Var "p") (Var "i"))),  -- p and i in scope in body
      testNeg (ForLoop ("p", CstInt 0) ("i", Var "i") (Var "p")),                   -- i not in scope in bound
      testNeg (ForLoop ("p", Var "p") ("i", CstInt 3) (Var "p")),                   -- p not in scope in initial

      testPos (Lambda "x" (Var "x")),                                               -- parameter in scope in body
      testNeg (Lambda "x" (Var "y")),                                               -- other names are not

      testNeg (Apply (Var "x") (CstInt 1)),
      testNeg (Apply (Lambda "y" (CstInt 1)) (Var "x")),

      testNeg (TryCatch (Var "x") (CstInt 1)),
      testNeg (TryCatch (CstInt 1) (Var "x")),                                      -- handler checked even if it never runs
    
      testNeg (Print "a" (Var "x")),
      
      testNeg (KvPut (Var "x") (CstInt 1)),
      testNeg (KvPut (CstInt 0) (Var "x")),

      testNeg (KvGet (Var "x"))
    ]