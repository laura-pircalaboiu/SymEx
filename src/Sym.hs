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

-- thought process: After each successful unification we'd like to add a constraint to the constraint list.
-- We want to also perform unification on pattern matches (i.e. if n fits the pattern of Num i)


data Patt = Num | Add String String
          deriving (Eq, Show)

data Term = Var String
          | NumV Int
          | SymV [Constraint] String
          | ConV String [Term]
          | FunV String String String
          deriving (Eq)

type Unifier  = [(String, Term)]

instance Show Term where
  show (Var a) = show a
  show (NumV n) = show n
  show (SymV const s) = "Symbolic " ++ show const ++ " " ++ show s
  show (ConV s xs) = show s ++ "[" ++ show xs ++ "]"
  show (FunV op x y) = "applying " ++ show op ++ " to " ++ show x ++ " and " ++ show y

data Constraint = Eq Term Term
                | NEq Term Term
                | Match Term Patt
                deriving (Eq)

instance Show Constraint where
  show (Eq a b) = show a ++ " == " ++ show b
  show (NEq a b) = show a ++ " != " ++ show b
  show (Match a b) = show a ++ " matches " ++ show b

data IR = Choice [IR]
        | Recurse String String IR
        | Guard [Constraint] IR
        | Return Term
        | Seq String IR IR
        | Raise


instance Show IR where
  show (Choice []) = ""
  show (Choice (x:xs)) = show x ++ "+" ++ show xs
  show (Recurse xs vr tree) = "recurse" ++ show xs ++ " as " ++ show vr ++ ". " ++ show tree
  show (Guard cs tree) = "[" ++ show cs ++ "]" ++ show tree
  show (Seq y e1 e2) = "seq " ++ show e1 ++ " as " ++ show y ++ " then " ++ show e2
  show (Return t) = "return " ++ show t
  show (Raise) = "error occured!"

data Prog = Prog [(String, [String], IR)]


-- purpose: testing definitional interpreters

-- method: symbolic execution

-- objective: implement a simple symbolic executor that does left-to-right breadth-first search of the branches of an IR

--
-- P = [ e = Num(i) ] . ...1
--  + [ e = Add(e1, e2) ] . ...2
--  + ...3
--
--  recurse e1 as NumV(i1) . M
--
--    [ e1 = Num(i) ] . ...1 ++ M
--  + [ e1 = Add(e1, e2) ] . ...2 ++ M
--  + ...3 ++ M
--
--
---------------------------------
--
--P = [ e = Num(i) ] . ...1
--  + [ e = Add(e1, e2) ] . ...2
--  + ...3
--
--  recurse e1 as NumV(i1) . M
--
--  data IR = ...
--
--  data InterpState = Single IR
--                   | Seq IR InterpState
--
--  step :: InterpState ->
--  step (


step :: Term -> IR -> IR -> [IR]
step e eval (Choice xs) = xs
step e eval (Guard [(Eq v@(SymV const s) y)] cont) = case unify v y of
                                        Just [] -> [cont]
                                        Just [(s, t)] -> [(substIR (SymV (const ++ [Eq v y]) s) cont), Raise]
                                        Nothing -> [Raise]

-- in this case we'd just like to know if there are any constraints that "force" variable x to be pattern p
step e eval (Guard [(Match x@(SymV const s) p)] cont) = case tryMatch x p of
                                           True -> [substIR (SymV (const ++ [Match x p]) s) cont]
                                           False -> [Raise]


step e eval (Guard [(NEq v@(SymV const s) y)] cont) = case unify v y of
                                        Nothing -> [cont]
                                        Just [(s, t)] -> [(substIR (SymV (const ++ [Eq v y]) s) cont), Raise]
                                        Just [] -> [Raise]

step e eval (Guard cs cont) = [cont]
step e eval (Recurse x y cont) = map (\branch -> Seq y branch cont) (step (SymV [] x) (Guard [(Eq (SymV [] "a") (NumV 3))] (Return (SymV [] "a"))) (Guard [(Eq (SymV [] "a") (NumV 3))] (Return (SymV [] "a"))))



-- TODO: test Seq e1 e2 case
-- where we define seq as unfolding e1 until a result is returned and used for the evaluation of e2

step e eval (Seq y e1 e2) = case stepped of
                               [Return x] -> step e eval (substRecur x y e2)
                               [Raise] -> [Raise]
                               st -> foldr (\x acc -> acc ++ [Seq y x e2]) [] st
                               where stepped = step e eval e1

step e eval (Return x) = [Return x] -- not necessary because ideally we'd like to stop here
step _ _ _ = [Raise]

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

unify :: Term -> Term -> Maybe [(String, Term)]
unify v1 v2 | v1 == v2 = return []
unify (ConV s1 vs1) (ConV s2 vs2) | s1 == s2 =
    foldr (\ (v1, v2) m -> do
              u <- m
              u' <- unify v1 v2
              return (u ++ u'))
          (return [])
          (zip vs1 vs2)

unify (ConV _ _)    (ConV _ _)  = Nothing
unify x@(ConV _ _)  y@(SymV const _)   = unify y x
unify (SymV const x)       t            | occurs x t = Nothing
unify (SymV const x)       v            = return [(x, v)]
unify _              _            = Nothing

occurs :: String -> Term -> Bool
--occurs x (SymV const y)      | x == y = True
--occurs _ (SymV const _)      = False
--occurs x (ConV _ vs)  =
--  foldl (\ b v -> b || occurs x v) False vs
occurs x y = False

tryMatch :: Term -> Patt -> Bool
--occurs x (SymV const y)      | x == y = True
--occurs _ (SymV const _)      = False
--occurs x (ConV _ vs)  =
--  foldl (\ b v -> b || occurs x v) False vs
tryMatch x y = True

subst :: Term -> Term -> Term
subst (SymV const1 x1) (SymV const2 x2)        = if x1 == x2 then (SymV const1 x1) else (SymV const2 x2)
subst s@(SymV const1 s1) (ConV s2 args2)  =
  ConV s2 (map (subst s) args2)

substIR :: Term -> IR -> IR
substIR x (Choice xs) = Choice (map (\br -> (substIR x br) ) xs)
substIR x (Guard cs cont) = Guard cs (substIR x cont)
substIR x (Recurse xs n cont) = Recurse xs n (substIR x cont)
substIR v@(SymV const1 x1) (Return (SymV const x)) = if x == x1 then (Return v) else (Return (SymV const x))
substIR v@(Var x1) (Return (Var x)) = if x1 == x then (Return v) else (Return (Var x))
substIR x y = y

substRecur :: Term -> String -> IR -> IR
substRecur x y (Choice xs) = Choice (map (\br -> (substRecur x y br)) xs)
substRecur x y(Guard cs cont) = Guard cs (substRecur x y cont)
substRecur x y (Recurse xs n cont) = Recurse xs n (substRecur x y cont)
substRecur v@(SymV const1 x1) y (Return (SymV const x)) = if x == y then (Return v) else (Return (SymV const x))
substRecur v@(Var x1) y (Return (Var x)) = if x1 == y then (Return v) else (Return (Var x))
substRecur x y z = z


-- driver functions

driver :: Term -> [IR] -> [IR]
driver _ [] = []
-- step the next interp in line
driver e (x:xs) = (step e x x) ++ (driver e xs)


runTimes :: Int -> Term -> [IR] -> [IR]
runTimes 1 e eval = driver e eval
runTimes n e eval = driver e (runTimes (n - 1) e eval)