module Symbolic
    ( stepSym
    ) where

import System.IO
import Data.Char

data Value = NumV Int | BoolV Bool | ClosV String Expr a | SymV Char Int

data Constraint = LtConstr Expr Expr

data Expr = Num Int
            | Add Expr Expr
            | Eq Expr Expr
            | Id String

data Cmd val :: * -> * where
  Add_c   ::  val -> val -> Cmd val val
  Lt_c   ::  val -> val -> Cmd val val
  Fail   ::  String -> Cmd val a

----newtype State = Environment -> Expr)
--
---- our symbolic state contains: A symbolic environment, a current path constraint, and a "next symbolic variable" int field for var generation
---- newtype SymState = (Maybe Value, SymEnvironment, PathConstraint, Int)
--data State c a  =  Stop a
--                   |  forall b. Step (c b) (b -> State c a)
--
---- newtype SymState = (Maybe Value, SymEnvironment, PathConstraint, Int) -> Expr
--
---- newtype Environment = [(String, Value)]
---- newtype SymEnvironment = [(String, SymValue)]
--
--stepConc :: State (Cmd ConcreteValue) -> State (Cmd ConcreteValue)
--
--stepConc (Step (Add_c a b) k) = return (k (ConV (a + b) []))
--
--stepConc (Step (Eq_c a b) k) | a == b = return (k (ConV "true" []))
--stepConc (Step (Eq_c _ _) k) = return (k (ConV "false" []))
--
--stepConc (Stop x) = return (Stop x)
--
--
--stepSym :: State (Cmd SymbolicValue) -> State (Cmd SymbolicValue)
--
--stepSym (Step (Add_c a b) k) = return (k (ConV (a + b) []))
--
---- stepSym (Step (Eq_c a b) k) = case unify a b of
----                              []
--
--stepSym (Stop x) = return (Stop x)
--



---- return number val
--interpSym (Num i) ([Nothing, env, pc, i)])= [(Just (NumV i), env, pc, i)]
--
---- return id val
--interpSym (Id x) ([Nothing, env, pc, i)])= [(Just (lookup x env), env, pc, i)]
--
--
---- recursively check if L and R evaluate to a Num expression and then generate all three cases (both nums, left not num, right not num)
--interpSym (Add i0 i1) s = case (interpSym i0 s, interpSym i1 s) of
--                              ([]) ->
----
----  [Right ([NumV (i0 + i1), ne, pc, i)] ++
----  -- if right is not a num
----  [Left "Right is not a Num"] ++
----  -- if left is not a num
----  [Left "Left is not a Num"]
--
---- for less than:
---- 1. we evaluate a and b and see if they can evaluate to numbers.
---- 2. we generate the true case and add a < b to the constraint list of this state.
---- 3. we generate the false case and add a > b to the constraint list of this state.
--interpSym (Lt a b) s = case (interpSym a s, interpSym b s) of -- tbd
--
--  -- if true
--  [(BoolV True, senv, (LtCon i0 i1) && pc, i)] ++
--  -- if false
--  [(BoolV False, senv, (LtCon i1 i0) && pc, i)]
--
--
---- helper functions
--
---- driver functions
