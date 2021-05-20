module Symbolic
    ( stepSym
    ) where

import System.IO
import Data.Char

data Value = NumV Int | BoolV Bool | ClosV String Expr a | SymV Char Int

data Expr = Num Int
            | Add Expr Expr
            | Lt Expr Expr
            | Id String

newtype State = Environment

-- our symbolic state contains: A symbolic environment, a current path constraint, and a "next symbolic variable" int field for var generation
newtype SymState = (Maybe Value, SymEnvironment, PathConstraint, Int)


newtype Environment = [(String, Value)]
newtype SymEnvironment = [(String, SymValue)]

--stepConc :: State -> Cmd -> Either String State
--stepConc nv (Add a1 a2) =

interpSym :: Expr -> SymState -> [Either String SymState]

-- return number val
interpSym (Num i) ([Nothing, env, pc, i)])= [(Just (NumV i), env, pc, i)]

-- return id val
interpSym (Id x) ([Nothing, env, pc, i)])= [(Just (lookup x env), env, pc, i)]


-- recursively check if L and R evaluate to a Num expression and then generate all three cases (both nums, left not num, right not num)
interpSym (Add i0 i1) s = case (interpSym i0 s, interpSym i1 s) of
                              ([]) ->
--
--  [Right ([NumV (i0 + i1), ne, pc, i)] ++
--  -- if right is not a num
--  [Left "Right is not a Num"] ++
--  -- if left is not a num
--  [Left "Left is not a Num"]

-- for less than: we'd like to generate the two possible branches (true or false) so we recur until hitting a Numeric value
interpSym (Lt i0 i1) (senv, pc, i) =


--  -- if true
--  [(BoolV True, senv, (LtCon i0 i1) && pc, i)] ++
--  -- if false
--  [(BoolV False, senv, (LtCon i1 i0) && pc, i)] ++
--  -- if right is not a num
--  [Left "Right is not a Num"] ++
--  -- if left is not a num
--  [Left "Left is not a Num"]

