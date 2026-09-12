module APL.Eval_Tests (tests) where

import APL.AST (Exp (..))
import APL.Eval (Val (..), envEmpty, eval)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

-- -- Consider this example when you have added the necessary constructors.
-- -- The Y combinator in a form suitable for strict evaluation.
-- yComb :: Exp
-- yComb =
--   Lambda "f" $
--     Apply
--       (Lambda "g" (Apply (Var "g") (Var "g")))
--       ( Lambda
--           "g"
--           ( Apply
--               (Var "f")
--               (Lambda "a" (Apply (Apply (Var "g") (Var "g")) (Var "a")))
--           )
--       )

-- fact :: Exp
-- fact =
--   Apply yComb $
--     Lambda "rec" $
--       Lambda "n" $
--         If
--           (Eql (Var "n") (CstInt 0))
--           (CstInt 1)
--           (Mul (Var "n") (Apply (Var "rec") (Sub (Var "n") (CstInt 1))))

tests :: TestTree
tests =
  testGroup
    "Evaluation"
    [ testCase "Add" $
        eval envEmpty (Add (CstInt 2) (CstInt 5))
          @?= Right (ValInt 7),
      --
      testCase "Add (wrong type)" $
        eval envEmpty (Add (CstInt 2) (CstBool True))
          @?= Left "Non-integer operand",
      --
      testCase "Sub" $
        eval envEmpty (Sub (CstInt 2) (CstInt 5))
          @?= Right (ValInt (-3)),
      --
      testCase "Div" $
        eval envEmpty (Div (CstInt 7) (CstInt 3))
          @?= Right (ValInt 2),
      --
      testCase "Div0" $
        eval envEmpty (Div (CstInt 7) (CstInt 0))
          @?= Left "Division by zero",
      --
      testCase "Pow" $
        eval envEmpty (Pow (CstInt 2) (CstInt 3))
          @?= Right (ValInt 8),
      --
      testCase "Pow0" $
        eval envEmpty (Pow (CstInt 2) (CstInt 0))
          @?= Right (ValInt 1),
      --
      testCase "Pow negative" $
        eval envEmpty (Pow (CstInt 2) (CstInt (-1)))
          @?= Left "Negative exponent",
      --
      testCase "Eql (false)" $
        eval envEmpty (Eql (CstInt 2) (CstInt 3))
          @?= Right (ValBool False),
      --
      testCase "Eql (true)" $
        eval envEmpty (Eql (CstInt 2) (CstInt 2))
          @?= Right (ValBool True),
      --
      testCase "If" $
        eval envEmpty (If (CstBool True) (CstInt 2) (Div (CstInt 7) (CstInt 0)))
          @?= Right (ValInt 2),
      --
      testCase "Let" $
        eval envEmpty (Let "x" (Add (CstInt 2) (CstInt 3)) (Var "x"))
          @?= Right (ValInt 5),
      --
      testCase "Let (shadowing)" $
        eval
          envEmpty
          ( Let
              "x"
              (Add (CstInt 2) (CstInt 3))
              (Let "x" (CstBool True) (Var "x"))
          )
          @?= Right (ValBool True),
          --

          testCase "ForLoop " $
        eval envEmpty (ForLoop ("p", CstInt 0) ("i", CstInt 10) (Add (Var "p") (Var ("i"))))
          @?= Right (ValInt 45),
        
      testCase "ForLoop body error " $
        eval envEmpty (ForLoop ("p", CstInt 5) ("i", CstInt 3) (Div (Var "p") (CstInt (0))))
          @?= Left "Division by zero",

      testCase "ForLoop Non-integral loop bound " $
        eval envEmpty (ForLoop ("p", CstInt 5) ("i", CstBool True) (Add (Var "p") (Var ("i"))))
          @?= Left "Non-integral loop bound",

      testCase "Lambda (ValFun conversion)" $ -- checks lambda expression is correctly turned into a ValFun
        eval envEmpty
          (Lambda "x" (Add (Var "x") (CstInt 1)))
          @?= Right (ValFun [] "x" (Add (Var "x") (CstInt 1))),

      testCase "Lambda environment" $ --test that Lambda remembers environment where it was created
        eval envEmpty
          (Let "x" (CstInt 5)
            (Lambda "y" (Add (Var "x") (Var "y"))))
          @?= Right (ValFun [("x", ValInt 5)] "y"
            (Add (Var "x") (Var "y"))),

      testCase "Apply (Function test)" $ --Test that a function can be applied to an argument and gives correct result
        eval envEmpty
          (Apply
            (Lambda "x" (Add (Var "x") (CstInt 1)))
            (CstInt 3))
          @?=Right (ValInt 4),
      
      testCase "Apply order " $ -- Tests that e1 is evaluated before e2 in Apply
        eval envEmpty
          (Apply
            (CstInt 5) 
            (Div (CstInt 1) (CstInt 0)))
          @?= Left "Invalid application",


      testCase "Apply diff. argument type" $ --Tests that Apply can use an argument of a different type (bool)
        eval envEmpty
          (Apply
            (Lambda "x" (Var "x"))
            (CstBool True))
          @?= Right (ValBool True),

      testCase "Apply captured environment" $ --Tests that applu uses the environment captured by the Lambda
        eval envEmpty
          (Apply
            (Let "x" (CstInt 2)
              (Lambda "y" (Add (Var "x") (Var "y"))))
            (CstInt 3))
          @?= Right (ValInt 5),

      testCase "TryCatch successful" $ -- Tests that TryCatch returns e1 when it succeeds
        eval envEmpty
          (TryCatch (CstInt 5) (CstInt 10))
          @?= Right (ValInt 5),

      testCase "TryCatch catches error" $ -- tests that TryCatch evaluates e2 when e1 fails
        eval envEmpty
          (TryCatch
            (Div (CstInt 1) (CstInt 0))
            (CstInt 10))
          @?= Right (ValInt 10)
    ]
-- TODO - add more
