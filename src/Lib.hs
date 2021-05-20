module Lib
    ( driver
    ) where

import System.IO
import Data.Char

driver = do
    contents <- readFile "interp.hs"
    writeFile "result.txt" (map toUpper contents)

data Term = TInt Int | TId Id | TFunc ([Term] -> Term) [Term]

data Tree = BranchTree (Either Tree Tree)
      | GuardedTree ([(Term, Term, (Term -> Term -> Boolean))] Tree)
      | RecurseTree Term TId Tree
      | ReturnTree Term

-- makes intermediate representation execution tree
makeTree :: String -> Tree

-- makes a branch or a leaf of the tree we are trying to build. Takes a String defining a branch of the def interpreter and a Term (symbolic inp).
makeLief :: String -> Term -> Tree
makeLief br e = do
                atoms <- splitOn "=" br
                return (atoms !! 0)