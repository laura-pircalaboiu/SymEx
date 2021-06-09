module Main where

import Sym

-- eval: Input: variable a [(Choice (Guard [(Eq (SymV "a") (Num i))] (Return (SymV "a"))
--                                  (Guard [(Eq (SymV "a") (Add i1 i2))] (Recur i1 "c" (Recur i2 "d" (Return (FunV "+" "c" "d"))))))]

-- evil: Input: variable a [(Choice (Guard [(Eq (SymV "a") (Num i))] (Return (SymV "a"))
--                                  (Guard [(Eq (SymV "a") (Add i1 i2))] (Recur i1 "c" (Recur i2 "d" (Return (FunV "+" "d" "d"))))))]

main :: IO ()
main = putStr (show (driver (SymV "a") [(Choice (Guard [(Eq (SymV "a") (Num i))] (Return (SymV "a")) (Guard [(Eq (SymV "a") (Add i1 i2))] (Recurse i1 "c" (Recurse i2 "d" (Return (FunV "+" "c" "d")))))))]))
