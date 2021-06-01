module Main where

import Sym

main :: IO ()
main = putStr (show (driver [Choice [Return (Num 3), Return (Num 4), Return (Num 6)], Raise]))
