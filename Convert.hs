module Convert where

import Lambda (Expr(..), BinOp(..), sample)
import CombinatorsData

translate :: Expr -> SKI
translate (Var x) = SVar x
translate (App e1 e2) = T (translate e1) (translate e2)
translate (Abs x e) = abstract x (translate e)
translate (Lit x) = SLit x

abstract :: Name -> SKI -> SKI
abstract x (T e1 e2) = combS (abstract x e1) (abstract x e2)
abstract x (SVar n) | x == n = I
abstract _ k = combK k

-- Show eta-reduction here...
combS :: SKI -> SKI -> SKI
combS f = T (T S f)

combK :: SKI -> SKI
combK = T K

desugar :: Expr -> Expr
desugar (App e1 e2) = App (desugar e1) (desugar e2)
desugar (Abs x e) = Abs x (desugar e)
desugar (Op op a b) = foldl App (Var n) args
  where
    args = map desugar [a,b]
    n = case op of
      Plus -> "$PLUS"
      Minus -> "$MINUS"
      Mul -> "$MUL"
      Div -> "$DIV"
desugar e = e

sampleSKI = translate (desugar sample)

eval' :: SKI -> SKI
eval' (T I x) = eval' x -- I x = x
eval' (T (T K x) y) = eval' x -- K x y = x
eval' (T (T (T S x) y) z) = eval' $ T ((T x z)) ((T y z)) -- S x y z = x z (y z)
eval' (T (T (SVar name) (SLit x)) (SLit y))
  | name `elem` ["$PLUS", "$MINUS", "$MUL", "$DIV"] = arith name x y
eval' (T x y) = T (eval' x) (eval' y) -- If we have (x y), evaluate x and y
eval' (SVar name) = SVar name
eval' p = p

arith :: Name -> Int -> Int -> SKI
arith "$PLUS"  x y = SLit (x + y)
arith "$MINUS" x y = SLit (x - y)
arith "$MUL"   x y = SLit (x * y)
arith "$DIV"   x y = SLit (x `div` y)

evalSample = eval' sampleSKI

sampleApplied = App (App sample (Lit 2)) (Lit 4)

evalSampleApplied = appUntil eval' $ translate $ desugar sampleApplied

appUntil f x = let y = f x in if x == y then x else appUntil f y
