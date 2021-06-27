module Main where


import Sym
import Test.HUnit

eval12 = Choice [(Guard [(Match (SymV [] "a") (Num))] (Return (SymV [] "a"))), (Guard [(Match (SymV [] "a") (Add "i1" "i2"))] (Recurse "i1" "c" (Recurse "i2" "d" (Return (OpV "+" "c" "d")))))]

eval' = Choice [(Guard [(Match (SymV [] "a") (Num))] (Return (SymV [] "a"))),
                (Guard [(Match (SymV [] "a") (Add "i1" "i2"))]
                  (Recurse "i1" "c"
                    (Recurse "i2" "d"
                      (Return (OpV "+" "d" "c"))))),
                (Guard [(Match (SymV [] "a") (Lam "arg" "body"))] (Recurse "arg" "c" (Recurse "body" "d" (Return (ClosV "arg" "body")))))]

eval'' = Choice [(Guard [(Match (SymV [] "a") (Num))] (Return (SymV [] "a"))),
                (Guard [(Match (SymV [] "a") (Add "i1" "i2"))]
                  (Recurse "i1" "c"
                    (Recurse "i2" "d"
                      (Return (OpV "+" "c" "d"))))),
                (Guard [(Match (SymV [] "a") (Lam "arg" "body"))] (Recurse "arg" "c" (Recurse "body" "d" (Return (ClosV "arg" "body")))))]

main :: IO ()
--main = putStr (show (driver (SymV [] "a") [(Guard [(Eq (SymV [] "a") (NumV 3))] (Return (SymV [] "a")))]))
--main = putStr (show (driver (SymV [] "a") (driver (SymV [] "a") [(Guard [(Match (SymV [] "a") (Add "i1" "i2"))] (Recurse "i1" "c" (Recurse "i2" "d" (Return (FunV "+" "c" "d")))))])))
--main = putStr (show (driver (SymV [] "a") (driver (SymV [] "a") (driver (SymV [] "a") [(Choice [(Guard [(Match (SymV [] "a") (Num))] (Return (SymV [] "a"))),
--                                           (Guard [(Match (SymV [] "a") (Add "i1" "i2"))] (Recurse "i1" "c" (Recurse "i2" "d" (Return (FunV "+" "c" "d")))))])]))))
--main = putStr (show (runTimes 3 (SymV [] "a") [(Choice [(Guard [(Match (SymV [] "a") (Num))] (Return (SymV [] "a"))),
--                                           (Guard [(Match (SymV [] "a") (Add "i1" "i2"))] (Recurse "i1" "c" (Recurse "i2" "d" (Return (FunV "+" "d" "c")))))]),
--                                           (Choice [(Guard [(Match (SymV [] "a") (Num))] (Return (SymV [] "a"))),
--                                           (Guard [(Match (SymV [] "a") (Add "i1" "i2"))] (Recurse "i1" "c" (Recurse "i2" "d" (Return (FunV "+" "c" "c")))))])]))
main = putStr (show (runTimes 3 (SymV [] "a") [eval12]))














--[return Symbolic [Symbolic [] "a" matches Num] "a",seq return Symbolic [Symbolic [] "a" == 3] "a" as "c" then recurse"i2" as "d". return applying "+" to "c" and "d",seq error occured! as "c" then recurse"i2" as
-- "d". return applying "+" to "c" and "d"]

--[return Symbolic [Symbolic [] "a" matches Num] "a",seq return Symbolic [Symbolic [] "a" == 3] "a" as "d" then return applying "+" to "c" and "d",seq error occured! as "d" then return applying "+" to "c" and "d"
--,error occured!]
