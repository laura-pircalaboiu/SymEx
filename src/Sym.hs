module Sym where

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


data SymVar = SymV String | ConV String [SymVar]
            deriving (Eq, Show)

type Unifier  = [(String, SymVar)]

data Term = Var String
          | Num Int
          | Con String [Term]

instance Show Term where
  show (Var a) = show a
  show (Num n) = show n
  show (Con s xs) = show s ++ "[" ++ show xs ++ "]"

data Constraint = Eq Term Term
                | NEq Term Term

instance Show Constraint where
  show (Eq a b) = show a ++ "==" ++ show b
  show (NEq a b) = show a ++ "!=" ++ show b

data IR = Choice [IR]
        | Recurse [Term] String IR
        | Guard [Constraint] IR
        | Return (Either Term SymVar)
        | Raise


instance Show IR where
  show (Choice []) = ""
  show (Choice (x:xs)) = show x ++ "+" ++ show xs
  show (Recurse xs vr tree) = "recurse" ++ show xs ++ " as " ++ show vr ++ ". " ++ show tree
  show (Guard cs tree) = "[" ++ show cs ++ "]" ++ show tree
  show (Return t) = "return " ++ show t
  show (Raise) = "error occured!"

data Prog = Prog [(String, [String], IR)]


-- purpose: testing definitional interpreters

-- method: symbolic execution

-- objective: implement a simple symbolic executor that does left-to-right breadth-first search of the branches of an IR

step :: IR -> [IR]
step (Choice xs) = xs
step (Guard (Eq x y) cont) = case unify x y of
                                  Just [] -> [cont]
                                  Just [u] -> [(substIR u cont), Raise]
                                  Nothing -> [Raise]
-- step (Guard cs cont) = [cont]
--step (Recurse xs x cont) = ???
step (Return x) = [Return x] -- not necessary because ideally we'd like to stop here
step _ = [Raise]

-- step :: IR -> [IR]
-- step (Choice xs) = xs
-- step (Guard cs cont) = [cont]
-- step (Recurse xs x cont) = [cont]
-- step (Return x) = [Return x] -- not necessary because ideally we'd like to stop here
-- step _ = [Raise]

--checkCons :: [Constraint] -> Bool
--checkCons [] = True
--checkCons (Eq a b):xs | a == b = True && checkCons xs
--checkCons (Eq a b):xs | a \= b = False
--checkCons (NEq a b):xs | a == b = False
--checkCons (NEq a b):xs | a \= b = True && checkCons xs


-- aux functions for unification

unify :: SymVar -> SymVar -> Maybe Unifier
unify v1 v2 | v1 == v2 = return []
unify (ConV s1 vs1) (ConV s2 vs2) | s1 == s2 =
    foldr (\ (v1, v2) m -> do
              u <- m
              u' <- unify v1 v2
              return (u ++ u'))
          (return [])
          (zip vs1 vs2)

unify (ConV _ _)    (ConV _ _)  = Nothing
unify x@(ConV _ _)  y@(SymV _)   = unify y x
unify (SymV x)       t            | occurs x t = Nothing
unify (SymV x)       v            = return [(x, v)]
unify _              _            = Nothing

occurs :: String -> SymVar -> Bool
occurs x (SymV y)      | x == y = True
occurs _ (SymV _)      = False
occurs x (ConV _ vs)  =
  foldl (\ b v -> b || occurs x v) False vs

subst :: (String, SymVar) -> SymVar -> SymVar
subst (y, v) (SymVar x)        = if x == y then v else (SymV x)
subst (x, v) (ConV s args)  =
  ConV s (map (subst (x, v)) args)

substIR :: (String, SymVar) -> IR -> IR
substIR x (Choice xs) = Choice (\i -> map substIR x i)
substIR x (Guard cs cont) = Guard cs (substIR x cont)
substIR x (Recurse xs n cont) = Recurse xs (substIR x cont)
subst (y, v) (Return (Right (SymVar x))) = if x == y then v else (SymV x)
substIR _ = [Raise]



-- driver functions

driver :: [IR] -> [IR]
driver [] = []
driver (x:xs) = (step x) ++ (driver xs)


