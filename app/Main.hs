module Main where

import Sym

main :: IO ()
--main = putStr (show (driver (SymV [] "a") [(Guard [(Eq (SymV [] "a") (NumV 3))] (Return (SymV [] "a")))]))
--main = putStr (show (driver (SymV [] "a") (driver (SymV [] "a") [(Guard [(Match (SymV [] "a") (Add "i1" "i2"))] (Recurse "i1" "c" (Recurse "i2" "d" (Return (FunV "+" "c" "d")))))])))
main = putStr (show (driver (SymV [] "a") (driver (SymV [] "a") (driver (SymV [] "a") [(Choice [(Guard [(Match (SymV [] "a") (Num))] (Return (SymV [] "a"))),
                                           (Guard [(Match (SymV [] "a") (Add "i1" "i2"))] (Recurse "i1" "c" (Recurse "i2" "d" (Return (FunV "+" "c" "d")))))])]))))
