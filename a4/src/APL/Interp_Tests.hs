module APL.Interp_Tests (tests) where

import APL.AST (Exp (..))
import APL.Eval (eval)
import APL.InterpIO (runEvalIO)
import APL.InterpPure (runEval)
import APL.Monad
import APL.Util (captureIO)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

eval' :: Exp -> ([String], Either Error Val)
eval' = runEval . eval

evalIO' :: Exp -> IO (Either Error Val)
evalIO' = runEvalIO . eval

tests :: TestTree
tests = testGroup "Free monad interpreters" [pureTests, ioTests]

pureTests :: TestTree
pureTests =
  testGroup
    "Pure interpreter"
    [ testCase "localEnv" $
        runEval
          ( localEnv (const [("x", ValInt 1)]) $
              askEnv
          )
          @?= ([], Right [("x", ValInt 1)]),
      --
      testCase "Let" $
        eval' (Let "x" (Add (CstInt 2) (CstInt 3)) (Var "x"))
          @?= ([], Right (ValInt 5)),
      --
      testCase "Let (shadowing)" $
        eval'
          ( Let
              "x"
              (Add (CstInt 2) (CstInt 3))
              (Let "x" (CstBool True) (Var "x"))
          )
          @?= ([], Right (ValBool True)),
      --
      testCase "Print" $
        runEval (evalPrint "test")
          @?= (["test"], Right ()),
      --
      testCase "Error" $
        runEval
          ( do
              _ <- failure "Oh no!"
              evalPrint "test"
          )
          @?= ([], Left "Oh no!"),
      --
      testCase "Div0" $
        eval' (Div (CstInt 7) (CstInt 0))
          @?= ([], Left "Division by zero")
          --- part 1 tests below
                ,
      testCase "TryCatch: success" $
        runEval (Free $ TryCatchOp (pure $ ValInt 5) (pure $ ValInt 1) pure)
          @?= ([], Right (ValInt 5)),
      --
      testCase "TryCatch: failure falls back" $
        runEval (Free $ TryCatchOp (failure "Oh no!") (pure $ ValInt 1) pure)
          @?= ([], Right (ValInt 1)),
      --
      testCase "TryCatch: keeps prints from m1" $
        runEval (Free $ TryCatchOp (evalPrint "a" >> failure "x") (pure $ ValInt 1) pure)
          @?= (["a"], Right (ValInt 1)),
      --
      testCase "TryCatch: continuation runs after fallback" $
        runEval (Free $ TryCatchOp (failure "x") (pure $ ValInt 1) (\v -> evalPrint "k" >> pure v))
          @?= (["k"], Right (ValInt 1)),
      --
      testCase "TryCatch via eval" $
        eval' (TryCatch (CstBool True `Eql` CstInt 0) (CstInt 1))
          @?= ([], Right (ValInt 1)),
          -- part 1 tests above
          -- part 2 tests below
      testCase "KvPut then KvGet" $
        runEval (Free $ KvPutOp (ValInt 0) (ValInt 1) (evalKvGet (ValInt 0)))
          @?= ([], Right (ValInt 1)),
      --
      testCase "KvPut replaces existing key" $
        runEval
          ( do
              evalKvPut (ValInt 0) (ValInt 1)
              evalKvPut (ValInt 0) (ValBool True)
              evalKvGet (ValInt 0)
          )
          @?= ([], Right (ValBool True)),
      --
      testCase "KvGet missing key" $
        runEval (evalKvGet (ValInt 0))
          @?= ([], Left "Invalid key: ValInt 0"),
      --
      testCase "KvGet distinguishes keys" $
        runEval (evalKvPut (ValInt 0) (ValInt 1) >> evalKvGet (ValInt 1))
          @?= ([], Left "Invalid key: ValInt 1"),
      -- part 2 tests above
      -- part 3 tests below
      testCase "Transaction commit" $
        eval' (Let "_" (Transaction (KvPut (CstInt 0) (CstInt 1))) (KvGet (CstInt 0)))
          @?= ([], Right (ValInt 1)),
      --
      testCase "Transaction rollback" $
        eval'
          ( TryCatch
              (Transaction (Let "_" (KvPut (CstInt 0) (CstBool False)) (Var "die")))
              (KvGet (CstInt 0))
          )
          @?= ([], Left "Invalid key: ValInt 0"),
      --
      testCase "Transaction propagates error" $
        eval' (Transaction (Let "_" (KvPut (CstInt 0) (CstBool False)) (Var "die")))
          @?= ([], Left "Unknown variable: die"),
      --
      testCase "Transaction keeps prints on failure" $
        runEval (transaction (evalPrint "weee" >> failure "oh no"))
          @?= (["weee"], Left "oh no"),
      --
      testCase "Transaction: nested 1" $
        eval'
          ( Let "_"
              ( Transaction
                  ( Let "_" (KvPut (CstInt 0) (CstInt 1)) $
                      TryCatch
                        (Transaction (Let "_" (KvPut (CstInt 0) (CstBool False)) (Var "die")))
                        (CstBool True)
                  )
              )
              (KvGet (CstInt 0))
          )
          @?= ([], Right (ValInt 1)),
      --
      testCase "Transaction: nested 2" $
        eval'
          ( Let "_"
              ( TryCatch
                  (Transaction (Transaction (Let "_" (KvPut (CstInt 0) (CstBool False)) (Var "die"))))
                  (CstBool True)
              )
              (KvGet (CstInt 0))
          )
          @?= ([], Left "Invalid key: ValInt 0"),
      -- part 3 tests above
      -- part 4 below
      testCase "Break exits loop (handout example)" $
        eval' (ForLoop ("p", CstInt 0) ("i", CstInt 100) (Let "_" (Break (CstBool True)) (Var "i")))
          @?= ([], Right (ValBool True)),
      --
      testCase "Break outside loop" $
        eval' (Break (CstBool True))
          @?= ([], Left "Break outside loop"),
      --
      testCase "Break stops at the right iteration" $
        eval'
          ( ForLoop ("found", CstInt 0) ("i", CstInt 100) $
              If (Eql (Var "i") (CstInt 7)) (Break (Var "i")) (Var "found")
          )
          @?= ([], Right (ValInt 7)),
      --
      testCase "Break is not caught by TryCatch" $
        eval' (ForLoop ("p", CstInt 0) ("i", CstInt 10) (TryCatch (Break (CstInt 7)) (CstInt 0)))
          @?= ([], Right (ValInt 7)),
      --
      testCase "Break only exits the innermost loop" $
        eval'
          ( ForLoop ("p", CstInt 0) ("i", CstInt 3) $
              ForLoop ("q", CstInt 0) ("j", CstInt 5) (Break (CstInt 1))
          )
          @?= ([], Right (ValInt 1)),
      --
      testCase "Error inside loop is still an error" $
        eval' (ForLoop ("p", CstInt 0) ("i", CstInt 3) (Div (CstInt 1) (CstInt 0)))
          @?= ([], Left "Division by zero"),
      --
      testCase "Loop without break runs to completion" $
        eval' (ForLoop ("acc", CstInt 0) ("i", CstInt 4) (Add (Var "acc") (Var "i")))
          @?= ([], Right (ValInt 6))
      -- part 4 tests above

    ]

ioTests :: TestTree
ioTests =
  testGroup
    "IO interpreter"
    [ testCase "print" $ do
        let s1 = "Lalalalala"
            s2 = "Weeeeeeeee"
        (out, res) <-
          captureIO [] $
            runEvalIO $ do
              evalPrint s1
              evalPrint s2
        (out, res) @?= ([s1, s2], Right ())
        -- part 1 tests below
              ,
      testCase "TryCatch IO: failure falls back" $ do
        res <-
          runEvalIO $
            Free $ TryCatchOp (failure "Oh no!") (pure $ ValInt 1) pure
        res @?= Right (ValInt 1),
      --
      testCase "TryCatch IO: success" $ do
        res <-
          runEvalIO $
            Free $ TryCatchOp (pure $ ValInt 5) (pure $ ValInt 1) pure
        res @?= Right (ValInt 5),
        -- part 1 tests above

        -- part 2 tests below
      testCase "IO KvPut then KvGet" $ do
        res <- runEvalIO $ evalKvPut (ValInt 0) (ValInt 1) >> evalKvGet (ValInt 0)
        res @?= Right (ValInt 1),
      --
      testCase "IO KvPut replaces existing key" $ do
        res <- runEvalIO $ do
          evalKvPut (ValInt 0) (ValInt 1)
          evalKvPut (ValInt 0) (ValBool True)
          evalKvGet (ValInt 0)
        res @?= Right (ValBool True),
      --
      testCase "IO missing key prompts" $ do
        (_, res) <- captureIO ["ValInt 1"] $ runEvalIO $ evalKvGet (ValInt 0)
        res @?= Right (ValInt 1),
      --
      testCase "IO missing key, bool replacement" $ do
        (_, res) <- captureIO ["ValBool True"] $ runEvalIO $ evalKvGet (ValInt 0)
        res @?= Right (ValBool True),
      --
      testCase "IO missing key, invalid input" $ do
        (_, res) <- captureIO ["lol"] $ runEvalIO $ evalKvGet (ValInt 0)
        res @?= Left "Invalid value input: lol",
      --
      testCase "IO replacement is not stored in DB" $ do
        (_, res) <- captureIO ["ValInt 1", "ValInt 2"] $ runEvalIO $ do
          _ <- evalKvGet (ValInt 0)
          evalKvGet (ValInt 0)
        res @?= Right (ValInt 2),
      -- part 2 tests above


      -- part 3 tests below
      testCase "IO Transaction commit" $ do
        res <- evalIO' (Let "_" (Transaction (KvPut (CstInt 0) (CstInt 1))) (KvGet (CstInt 0)))
        res @?= Right (ValInt 1),
      --
      testCase "IO Transaction rollback" $ do
        (_, res) <-
          captureIO ["ValInt 9"] $
            evalIO'
              ( Let "_"
                  ( TryCatch
                      (Transaction (Let "_" (KvPut (CstInt 0) (CstBool False)) (Var "die")))
                      (CstBool True)
                  )
                  (KvGet (CstInt 0))
              )
        res @?= Right (ValInt 9), -- key 0 was rolled back, so we get prompted
      --
      testCase "IO Transaction propagates error" $ do
        res <- evalIO' (Transaction (Let "_" (KvPut (CstInt 0) (CstBool False)) (Var "die")))
        res @?= Left "Unknown variable: die",
      --
      testCase "IO Transaction keeps prints on failure" $ do
        (out, res) <-
          captureIO [] $
            runEvalIO (transaction (evalPrint "weee" >> failure "oh no"))
        (out, res) @?= (["weee"], Left "oh no"),
      -- part 3 tests above
      -- part 4 tests below
      testCase "IO Break exits loop (handout example)" $ do
        res <- evalIO' (ForLoop ("p", CstInt 0) ("i", CstInt 100) (Let "_" (Break (CstBool True)) (Var "i")))
        res @?= Right (ValBool True),
      --
      testCase "IO Break outside loop" $ do
        res <- evalIO' (Break (CstBool True))
        res @?= Left "Break outside loop",
      --
      testCase "IO Break is not caught by TryCatch" $ do
        res <- evalIO' (ForLoop ("p", CstInt 0) ("i", CstInt 10) (TryCatch (Break (CstInt 7)) (CstInt 0)))
        res @?= Right (ValInt 7)
      -- part 4 tests above



        -- NOTE: This test will give a runtime error unless you replace the
        -- version of `eval` in `APL.Eval` with a complete version that supports
        -- `Print`-expressions. Uncomment at your own risk.
        -- testCase "print 2" $ do
        --    (out, res) <-
        --      captureIO [] $
        --        evalIO' $
        --          Print "This is also 1" $
        --            Print "This is 1" $
        --              CstInt 1
        --    (out, res) @?= (["This is 1: 1", "This is also 1: 1"], Right $ ValInt 1)
    ]
