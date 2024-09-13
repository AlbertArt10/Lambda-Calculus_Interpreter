module Lambda where

import Data.List (nub, (\\))

data Lambda = Var String
            | App Lambda Lambda
            | Abs String Lambda
            | Macro String

instance Show Lambda where
    show (Var x) = x
    show (App e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
    show (Abs x e) = "λ" ++ x ++ "." ++ show e
    show (Macro x) = x

instance Eq Lambda where
    e1 == e2 = eq e1 e2 ([],[],[])
      where
        eq (Var x) (Var y) (env,xb,yb) = elem (x,y) env || (not $ elem x xb || elem y yb)
        eq (App e1 e2) (App f1 f2) env = eq e1 f1 env && eq e2 f2 env
        eq (Abs x e) (Abs y f) (env,xb,yb) = eq e f ((x,y):env,x:xb,y:yb)
        eq (Macro x) (Macro y) _ = x == y
        eq _ _ _ = False

-- 1.1.
vars :: Lambda -> [String]
vars (Var x) = [x]
vars (App e1 e2) = nub (vars e1 ++ vars e2)
vars (Abs x e) = nub (x : vars e)
vars (Macro _) = []

-- 1.2.
freeVars :: Lambda -> [String]
freeVars (Var x) = [x]
freeVars (App e1 e2) = nub (freeVars e1 ++ freeVars e2)
freeVars (Abs x e) = freeVars e \\ [x]
freeVars (Macro _) = []

-- 1.3.
newVar :: [String] -> String
newVar vars = head (filter (`notElem` vars) candidates)
  where
    candidates = [ [a] | a <- ['a'..'z']] ++ [ a : b | b <- candidates, a <- ['a'..'z']]

-- 1.4.
isNormalForm :: Lambda -> Bool
isNormalForm (Var _) = True
isNormalForm (Abs _ e) = isNormalForm e
isNormalForm (App (Abs _ _) _) = False
isNormalForm (App e1 e2) = isNormalForm e1 && isNormalForm e2
isNormalForm (Macro _) = True

-- 1.5.
reduce :: String -> Lambda -> Lambda -> Lambda
reduce x e1 e2 = subst x e2 e1
  where
    subst var val (Var v)
      | v == var = val
      | otherwise = Var v
    subst var val (App e1 e2) = App (subst var val e1) (subst var val e2)
    subst var val (Abs v e)
      | v == var = Abs v e
      | v `elem` freeVars val = let newV = newVar (vars e ++ vars val)
                                in Abs newV (subst var val (subst v (Var newV) e))
      | otherwise = Abs v (subst var val e)
    subst _ _ (Macro x) = Macro x

-- 1.6.
normalStep :: Lambda -> Lambda
normalStep (App (Abs x e1) e2) = reduce x e1 e2
normalStep (App e1 e2)
  | isNormalForm e1 = App e1 (normalStep e2)
  | otherwise = App (normalStep e1) e2
normalStep (Abs x e) = Abs x (normalStep e)
normalStep e = e

-- 1.7.
applicativeStep :: Lambda -> Lambda
applicativeStep (App e1 e2)
  | isNormalForm e1 && isNormalForm e2 = case e1 of
      Abs x e -> reduce x e e2
      _ -> App e1 e2
  | isNormalForm e1 = App e1 (applicativeStep e2)
  | otherwise = App (applicativeStep e1) e2
applicativeStep (Abs x e) = Abs x (applicativeStep e)
applicativeStep e = e

-- 1.8.
simplify :: (Lambda -> Lambda) -> Lambda -> [Lambda]
simplify step e
  | isNormalForm e = [e]
  | otherwise = e : simplify step (step e)

normal :: Lambda -> [Lambda]
normal = simplify normalStep

applicative :: Lambda -> [Lambda]
applicative = simplify applicativeStep
