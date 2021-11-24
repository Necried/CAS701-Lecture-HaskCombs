module Lambda where

type Name = String

data Expr
  = Var Name
  | App Expr Expr
  | Abs Name Expr
  | Lit Int
  | Op BinOp Expr Expr
  deriving (Show, Eq)

data BinOp
  = Plus
  | Minus
  | Mul
  | Div
  deriving (Show, Eq)

sample :: Expr
sample = Abs "x" (Abs "y"
  (Op Plus (Op Mul (Lit 3) (Var "x")) (Var "y")))
