module Main where

import Sym

main :: IO ()
main = putStr (show (step (Guard [(Eq (Num 3) (Num 3))] (Return (Num 3)))))
