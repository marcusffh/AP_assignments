module APL.Tests
  ( properties
  )
where

import APL.AST (Exp (..), VName, subExp, printExp)
import APL.Parser (parseAPL)
import APL.Error (isVariableError, isDomainError, isTypeError)
import APL.Check (checkExp)
import APL.Eval (eval, runEval)
import Test.QuickCheck
  ( Property
  , Gen
  , Arbitrary (arbitrary, shrink)
  , property
  , cover
  , checkCoverage
  , oneof
  , sized
  , withMaxSuccess
  , elements
  , frequency
  )

instance Arbitrary Exp where
  arbitrary = sized (genExp [])

  shrink (Add e1 e2) =
    e1 : e2 : [Add e1' e2 | e1' <- shrink e1] ++ [Add e1 e2' | e2' <- shrink e2]
  shrink (Sub e1 e2) =
    e1 : e2 : [Sub e1' e2 | e1' <- shrink e1] ++ [Sub e1 e2' | e2' <- shrink e2]
  shrink (Mul e1 e2) =
    e1 : e2 : [Mul e1' e2 | e1' <- shrink e1] ++ [Mul e1 e2' | e2' <- shrink e2]
  shrink (Div e1 e2) =
    e1 : e2 : [Div e1' e2 | e1' <- shrink e1] ++ [Div e1 e2' | e2' <- shrink e2]
  shrink (Pow e1 e2) =
    e1 : e2 : [Pow e1' e2 | e1' <- shrink e1] ++ [Pow e1 e2' | e2' <- shrink e2]
  shrink (Eql e1 e2) =
    e1 : e2 : [Eql e1' e2 | e1' <- shrink e1] ++ [Eql e1 e2' | e2' <- shrink e2]
  shrink (If cond e1 e2) =
    e1 : e2 : [If cond' e1 e2 | cond' <- shrink cond] ++ [If cond e1' e2 | e1' <- shrink e1] ++ [If cond e1 e2' | e2' <- shrink e2]
  shrink (Let x e1 e2) =
    e1 : [Let x e1' e2 | e1' <- shrink e1] ++ [Let x e1 e2' | e2' <- shrink e2]
  shrink (Lambda x e) =
    [Lambda x e' | e' <- shrink e]
  shrink (Apply e1 e2) =
    e1 : e2 : [Apply e1' e2 | e1' <- shrink e1] ++ [Apply e1 e2' | e2' <- shrink e2]
  shrink (TryCatch e1 e2) =
    e1 : e2 : [TryCatch e1' e2 | e1' <- shrink e1] ++ [TryCatch e1 e2' | e2' <- shrink e2]
  shrink _ = []

shortVar :: Gen VName
shortVar = elements ["ab", "foo", "test"]


genExp :: [VName] -> Int -> Gen Exp
genExp vars 0 = oneof [CstInt <$> arbitrary, CstBool <$> arbitrary]
genExp vars size =
  frequency $
    [ (5, CstInt <$> arbitrary)
    , (5, CstBool <$> arbitrary)
    , (5, Add <$> genExp vars halfSize <*> genExp vars halfSize)
    , (5, Sub <$> genExp vars halfSize <*> genExp vars halfSize)
    , (5, Mul <$> genExp vars halfSize <*> genExp vars halfSize)
    , (3, Div <$> genExp vars halfSize <*> genExp vars halfSize)
    , (3, Pow <$> genExp vars halfSize <*> genExp vars halfSize)
    , (5, Eql <$> genExp vars halfSize <*> genExp vars halfSize)
    , (5, If <$> genExp vars thirdSize <*> genExp vars thirdSize <*> genExp vars thirdSize)
    , (1, pure (Var "abcde"))

    , (1 ,
        do 
          v <- shortVar
          e1 <- genExp vars halfSize
          e2 <- genExp (v : vars) halfSize
          pure (Let v e1 e2)
      )
    , (1,  
        do 
          v <- shortVar
          e <- genExp (v : vars) (size - 1)
          pure (Lambda v e)
      )
    , (5,  
        do 
          v <- shortVar
          pure (Let v (CstInt 5) (Var v))
      )
    , (1, Apply <$> genExp vars halfSize <*> genExp vars halfSize)
    , (1, TryCatch <$> genExp vars halfSize <*> genExp vars halfSize)
    ]
    ++ if null vars 
      then []
      else [(15, Var <$> elements vars)]
  where
    halfSize = size `div` 2
    thirdSize = size `div` 3

expCoverage :: Exp -> Property
expCoverage e = checkCoverage
  . cover 20 (any isDomainError (checkExp e)) "domain error"
  . cover 20 (not $ any isDomainError (checkExp e)) "no domain error"
  . cover 20 (any isTypeError (checkExp e)) "type error"
  . cover 20 (not $ any isTypeError (checkExp e)) "no type error"
  . cover 5 (any isVariableError (checkExp e)) "variable error"
  . cover 70 (not $ any isVariableError (checkExp e)) "no variable error"
  . cover 50 (or [2 <= n && n <= 4 | Var v <- subExp e, let n = length v]) "non-trivial variable"
  $ ()

parsePrinted :: Exp -> Bool
parsePrinted e =
  parseAPL "" (printExp e) == Right e

onlyCheckedErrors :: Exp -> Bool-- function takes an expression and returns a bool
onlyCheckedErrors e =
  case runEval (eval e) of
    Left err -> err `elem` checkExp e --failed evaluation
    Right _ -> True -- succesful evaluation
-- The function. in the case of an error, checks if the true evaluated error, are present in the checked(predicted) errors, if so it returns True


-- temp helper
testCounterexample :: Bool
testCounterexample =
  onlyCheckedErrors
    (Apply
      (TryCatch
        (Lambda "x" (Div (CstInt 1) (CstInt 0)))
        (Lambda "x" (CstInt 1)))
      (CstInt 0))



-- The number of tests is part of the specification of this test suite: some of
-- these properties fail only rarely.  Do not reduce it.
properties :: [(String, Property)]
properties =
  [ ("expCoverage", property $ withMaxSuccess 10000 expCoverage)
  , ("parsePrinted", property $ withMaxSuccess 10000 parsePrinted)
  , ("onlyCheckedErrors", property $ withMaxSuccess 10000 onlyCheckedErrors)
  ]
