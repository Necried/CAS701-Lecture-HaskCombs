module CombinatorsData where

type Name = String -- This just type aliases Name to String

data SKI =
    I
  | K
  | S
  | SVar Name  -- in OCaml, this is like ~Var of Name~
  | T SKI SKI -- in OCaml, this is like ~T of SKI*SKI~
  | SLit Int   -- We'll use this later...
  deriving (Eq)

eval :: SKI -> SKI
eval (T I x) = eval x -- I x = x
eval (T (T K x) y) = eval x -- K x y = x
eval (T (T (T S x) y) z) = eval $ T ((T x z)) ((T y z)) -- S x y z = x z (y z)
eval (T x y) = T (eval x) (eval y) -- If we have (x y), evaluate x and y
eval (SVar name) = SVar name -- We don't have to evaluate variables
eval p = error $ "pattern failed to evaluate: " <> show p

instance Show SKI where
  show I = "I"
  show K = "K"
  show S = "S"
  show (SVar x) = x
  show (T x y) = "(" <> show x <> show y <> ")"
  show (SLit i) = "(" <> show i <> ")"

skkx = (T (T (T S K) K) (SVar "x")) -- (((S K) K) x)
print_skkx = print skkx
eval_skkx = eval skkx
