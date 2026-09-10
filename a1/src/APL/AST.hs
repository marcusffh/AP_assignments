module APL.AST
  ( VName,
    Exp (..),
    printExp,
  )
where

type VName = String

data Exp
  = CstInt Integer
  | CstBool Bool
  | Add Exp Exp
  | Sub Exp Exp
  | Mul Exp Exp
  | Div Exp Exp
  | Pow Exp Exp
  | Eql Exp Exp
  | If Exp Exp Exp
  | Var VName
  | Let VName Exp Exp
  | ForLoop (VName, Exp) (VName, Exp) Exp
  | Lambda VName Exp
  | Apply Exp Exp 
  | TryCatch Exp Exp
  deriving (Eq, Show)

printExp :: Exp -> String --type for printExp
printExpApplyArg :: Exp -> String --type for Apply helper function
printExpApplyArg e2 =
  case e2 of
    CstInt _ -> printExp e2
    CstBool _ -> printExp e2
    Var _ -> printExp e2
    _ -> "(" ++ printExp e2 ++ ")"
-- PrettyPrint for each case in data Exp

printExp (CstInt x) = show x
printExp (CstBool True) = "true"
printExp (CstBool False) = "false"

printExp (Add e1 e2) = printExp e1 ++ " + " ++ printExp e2
printExp (Sub e1 e2) = printExp e1 ++ " - " ++ printExp e2
printExp (Mul e1 e2) = printExp e1 ++ " * " ++ printExp e2
printExp (Div e1 e2) = printExp e1 ++ " / " ++ printExp e2
printExp (Pow e1 e2) = printExp e1 ++ " ** "++ printExp e2
printExp (Eql e1 e2) = printExp e1 ++ " == " ++ printExp e2
printExp (If e1 e2 e3) = "if " ++ printExp e1 ++ " then " ++ printExp e2 ++ " else " ++ printExp e3
printExp (Var x) = x

printExp (Let x e1 e2) = "let " ++ x ++ " = " ++ printExp e1 ++ " in " ++ printExp e2
printExp (ForLoop (x, e1) (y, e2) e3) = 
  "loop " ++ x ++ " = " ++ printExp e1 ++ " for "
   ++ y ++ " < "  ++ printExp e2 ++ " do " ++ printExp e3
printExp (Lambda x e1) = "\\" ++ x ++ " -> " ++ printExp e1
printExp (Apply e1 e2) =  
  case e1 of
    CstInt _ -> printExp e1 ++ " " ++ printExpApplyArg e2
    CstBool _ -> printExp e1 ++ " " ++ printExpApplyArg e2
    Var _ -> printExp e1 ++ " " ++ printExpApplyArg e2
    Apply _ _ -> printExp e1 ++ " " ++ printExpApplyArg e2
    _ -> "(" ++ printExp e1 ++ ")" ++ " " ++ printExpApplyArg e2

printExp (TryCatch e1 e2) = "try " ++ printExp e1 ++ " catch " ++ printExp e2
