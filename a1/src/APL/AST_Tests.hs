module APL.AST_Tests (tests) where

import APL.AST (Exp (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Prettyprinting"
    [ testCase " integer" $ printExp (CstInt 42) @?= "42",
    --
    testCase " boolean true" $ printExp (CstBool True) @?= "true",
    --
    testCase " boolean false" $ printExp (CstBool False) @?= "false",
    --
    testCase " Var " $ printExp (Var "x") @?= "x",
    --
    testCase " Add " $ printExp (Add (CstInt 5) (CstInt 5)) @?= "5 + 5",
    --
    testCase " Sub " $ printExp (Sub (CstInt 5) (CstInt 5)) @?= "5 - 5",
    --
    testCase " Mul " $ printExp (Mul (CstInt 5) (CstInt 5)) @?= "5 * 5",
    --
    testCase " Div " $ printExp (Div (CstInt 5) (CstInt 5)) @?= "5 / 5", 
    --
    testCase " Pow " $ printExp (Pow (CstInt 5) (CstInt 5)) @?= "5 ** 5",
    --
    testCase " Eql " $ printExp (Eql (CstInt 5) (CstInt 5)) @?= "5 == 5",
    --
    testCase " If " $ printExp (If (CstBool True) (CstInt 1) (CstInt 0)) @?= "if true then 1 else 0",
    --
    testCase " Let " $ printExp (Let "x" (CstInt 5) (Var "x")) @?= "let x = 5 in x",
    --
    testCase " ForLoop " $ printExp (ForLoop ("p", CstInt 0) ("i", CstInt 10) (Var "p")) @?= "loop p = 0 for i < 10 do p",
    --
    testCase " Lambda " $ printExp (Lambda "x" (Add (Var "x") (CstInt 1))) @?= "\\x -> x + 1",
    --
    testCase " Apply variable function " $ printExp (Apply (Var "f") (Var "x")) @?= "f x",
    --
    testCase " Apply Complex argument " $ printExp (Apply (Var "f") (Add (Var "x") (CstInt 1))) @?= "f (x + 1)",
    --
    testCase " Apply Complex function " $ printExp (Apply (Add (Var "f") (CstInt 1)) (Var "x")) @?= "(f + 1) x",
    --
    testCase " Apply as function " $ printExp (Apply (Apply (Var "f") (Var "x")) (Var "y")) @?= "f x y",
    --
    testCase " Apply constant function " $ printExp (Apply (CstInt 5) (Var "x")) @?= "5 x",
    --
    testCase " TryCatch " $ printExp (TryCatch (CstInt 5) (CstInt 5)) @?= "try 5 catch 5"
    ]
--Test for PrettyPrint defined in Ast.hs
