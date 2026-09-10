module APL.AST
  ( VName,
    Exp (..),
    printExp,
  )
where

type VName = String

data Exp -- We define a new data type that is either a... or...or
  = CstInt Integer
  | CstBool Bool
  | Add Exp Exp -- a + b
  | Sub Exp Exp -- a - b
  | Mul Exp Exp -- a * b
  | Div Exp Exp -- a / b
  | Pow Exp Exp -- a ^ b
  | Eql Exp Exp -- a == b
  | If Exp Exp Exp -- condition
  | Var VName -- a string
  | Let VName Exp Exp -- ...
  | ForLoop (VName, Exp) (VName, Exp) Exp
  | Lambda VName Exp
  | Apply Exp Exp 
  | TryCatch Exp Exp
  deriving (Eq, Show)

 -- Notice that the data type expression is defined
 -- recursively


printExp :: Exp -> String
printExp = undefined -- TODO
 
