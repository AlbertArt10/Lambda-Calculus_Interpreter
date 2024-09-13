module Default where

import Lambda
import Binding

-- Variables (for convenience)
vx = Var "x"
vy = Var "y"
vz = Var "z"
vf = Var "f"
vg = Var "g"
vh = Var "h"
vm = Var "m"
vn = Var "n"
vp = Var "p"
vq = Var "q"

-- Basic combinators
m = Abs "x" $ App vx vx
i = Abs "x" $ vx
k = Abs "x" $ Abs "y" $ vx
ki = Abs "x" $ Abs "y" $ vy
c = Abs "x" $ Abs "y" $ Abs "z" $ App (App vx vz) vy
y = Abs "f" $ App fix fix
  where fix = Abs "x" $ App vf (App vx vx)

-- 4.1. Boolean encodings
bTrue :: Lambda
bTrue = Abs "x" $ Abs "y" $ vx

bFalse :: Lambda
bFalse = Abs "x" $ Abs "y" $ vy

bAnd :: Lambda
bAnd = Abs "p" $ Abs "q" $ App (App vp vq) bFalse

bOr :: Lambda
bOr = Abs "p" $ Abs "q" $ App (App vp bTrue) vq

bNot :: Lambda
bNot = Abs "p" $ App (App vp bFalse) bTrue

bXor :: Lambda
bXor = Abs "p" $ Abs "q" $ App (App bOr (App (App bAnd vp) (App bNot vq))) (App (App bAnd (App bNot vp)) vq)

-- 4.2. Pair encodings
pair :: Lambda
pair = Abs "x" $ Abs "y" $ Abs "f" $ App (App vf vx) vy

first :: Lambda
first = Abs "p" $ App vp bTrue

second :: Lambda
second = Abs "p" $ App vp bFalse

-- 4.3. Natural number encodings
n0 :: Lambda
n0 = Abs "f" $ Abs "x" $ vx

n1 :: Lambda
n1 = Abs "f" $ Abs "x" $ App vf vx

n2 :: Lambda
n2 = Abs "f" $ Abs "x" $ App vf (App vf vx)

nSucc :: Lambda
nSucc = Abs "n" $ Abs "f" $ Abs "x" $ App vf (App (App vn vf) vx)

nPred :: Lambda
nPred = Abs "n" $ Abs "f" $ Abs "x" $
  App (App (App (Var "n")
                (Abs "g" $ Abs "h" $ App (Var "h") (App (Var "g") vf)))
           (Abs "u" vx))
      (Abs "u" (Var "u"))

nAdd :: Lambda
nAdd = Abs "m" $ Abs "n" $ Abs "f" $ Abs "x" $ App (App vm vf) (App (App vn vf) vx)

nSub :: Lambda
nSub = Abs "m" $ Abs "n" $ App (App vn nPred) vm


nMult :: Lambda
nMult = Abs "m" $ Abs "n" $ Abs "f" $ Abs "x" $ App (App vm (App vn vf)) vx

-- Default Context
defaultContext :: Context
defaultContext = 
    [ ("M", m)
    , ("I", i)
    , ("K", k)
    , ("KI", ki)
    , ("C", c)
    , ("Y", y)
    , ("TRUE", bTrue)
    , ("FALSE", bFalse)
    , ("AND", bAnd)
    , ("OR", bOr)
    , ("NOT", bNot)
    , ("XOR", bXor)
    , ("PAIR", pair)
    , ("FST", first)
    , ("SND", second)
    , ("N0", n0)
    , ("N1", n1)
    , ("N2", n2)
    , ("SUCC", nSucc)
    , ("PRED", nPred)
    , ("ADD", nAdd)
    , ("SUB", nSub)
    , ("MULT", nMult)
    ]
