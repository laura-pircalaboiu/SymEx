module Laura where

{-

-- objective: generate a term that exercises each path through the program.

-- assessment: seems interesting, but challenging

eval(e, nv) = ( [e ≡ Num(i)]. return t )
            + ( [e ̸≡ Num(i), e ≡ Add(e1, e2)]
              . recurse (e1, nv) as i1
              . recurse (e2, nv) as i2
              . return +(i1, i2)
              )
            + ...


Explore(eval)

->

eval(?e, ?nv)

  ( [?e ≡  Num(i)]. return NumV(i) )
+ ( [?e  ̸≡ Num(i), ?e ≡ Add(e1, e2)]
  . recurse (e1, nv) as v1
  . recurse (e2, nv) as v2
  . [pair(v1, v2) ≡ pair(NumV(i1), NumV(i2))]
  . return NumV(+(i1, i2))
  )

----------------------------------------------------
|
|--> {?e := Num(?i)}
|    |- return NumV(?i)
|       => choose an ?i
|
|--> {?e := Add(?e1, ?e2)}
     |- recurse (e1, nv) as v1
      . recurse (e2, nv) as v2
      . [pair(v1, v2) ≡ pair(NumV(i1), NumV(i2))]
      . return NumV(+(i1, i2))

     {?e := Add(?e1, ?e2)}
     [ recurse (?e1, ?nv) as ?v1
     , recurse (?e2, ?nv) as ?v2 ]
     |- [pair(v1, v2) ≡ pair(NumV(i1), NumV(i2))]
      . return NumV(+(i1, i2))

     {?e := Add(?e1, ?e2), ?v1 := NumV(?i1), ?v2 := NumV(?i2) }
     [ recurse (?e1, ?nv) == NumV(?i1)
     , recurse (?e2, ?nv) == NumV(?i2) ]
     |- return NumV(+(?i1, ?i2))

     eval(?e1, ?nv) == NumV(?i1)
     ---------------------------

     |- Num(?i1) -- ... choose the shallowest branch

     eval(?e2, ?nv) == NumV(?i2)
-}


-- data Expr = Add Expr Expr
--           | Num Int
--           | Lam String Expr
--           | App Expr Expr
--           | Ident String


data Term = Var String
          | Num Int
          | Con String [Term]

data Constraint = Eq Term Term
                | NEq Term Term

data IR = Choice [IR]
        | Recurse [Term] String IR
        | Guard [Constraint] IR
        | Return Term
        | Raise

data Prog = Prog [(String, [String], IR)]


-- purpose: testing definitional interpreters

-- method: symbolic execution

-- objective: implement a simple symbolic executor that does left-to-right breadth-first search of the branches of an IR

step :: IR -> [IR]
step (Choice a b) = [b]
step (Guard cs cont) | checkCons cs = [cont]
step (Recurse xs x cont) = [cont]
step (Return x) = [Return x] -- not necessary because ideally we'd like to stop here


checkCons :: [Constraint] -> Bool
checkCons [] = True
checkCons (Eq a b):xs | a == b = True && checkCons xs
checkCons (Eq a b):xs | a \= b = False
checkCons (NEq a b):xs | a == b = False
checkCons (NEq a b):xs | a \= b = True && checkCons xs


driver :: [IR] -> [IR]

