module Main where

import Sym
import Test.HUnit
import System.Exit


eval11 = Choice [(Guard [(Match (SymV [] "a") (Num))] (Return (SymV [] "a"))), (Guard [(Match (SymV [] "a") (Add "i1" "i2"))] (Recurse "i1" "c" (Recurse "i2" "d" (Return (OpV "+" "d" "c")))))]
eval12 = Choice [(Guard [(Match (SymV [] "a") (Num))] (Return (SymV [] "a"))), (Guard [(Match (SymV [] "a") (Add "i1" "i2"))] (Recurse "i1" "c" (Recurse "i2" "d" (Return (OpV "+" "c" "d")))))]
eval13 = Choice [(Guard [(Match (SymV [] "a") (Add "i1" "i2"))] (Recurse "i1" "c" (Recurse "i2" "d" (Return (OpV "+" "c" "d"))))), (Guard [(Match (SymV [] "a") (Num))] (Return (SymV [] "a")))]

eval21 = Choice [(Guard [(Match (SymV [] "a") (Num))] (Return (SymV [] "a"))), (Guard [(Match (SymV [] "a") (Add "i1" "i2"))] (Recurse "i1" "c" (Recurse "i2" "d" (Return (OpV "+" "c" "c")))))]
eval22 = Choice [(Guard [(Match (SymV [] "a") (Num))] (Return (SymV [] "a"))), (Guard [(Match (SymV [] "a") (Add "i1" "i2"))] (Recurse "x1" "c" (Recurse "i2" "d" (Return (OpV "+" "c" "c")))))]
eval23 = Choice [(Guard [(Match (SymV [] "a") (Num))] (Return (SymV [] "a"))), (Guard [(Match (SymV [] "a") (Add "i1" "i2"))] (Recurse "x1" "c" (Recurse "i2" "d" (Return (OpV "+" "d" "c")))))]


eval31 = Choice [(Guard [(Match (SymV [] "a") (Num))] (Return (SymV [] "a"))),
                (Guard [(Match (SymV [] "a") (Add "i1" "i2"))]
                  (Recurse "i1" "c"
                    (Recurse "i2" "d"
                      (Return (OpV "+" "d" "c"))))),
                (Guard [(Match (SymV [] "a") (Lam "arg" "body"))] (Recurse "arg" "c" (Recurse "body" "d" (Return (ClosV "arg" "body")))))]

eval32 = Choice [(Guard [(Match (SymV [] "a") (Num))] (Return (SymV [] "a"))),
                (Guard [(Match (SymV [] "a") (Add "i1" "i2"))]
                  (Recurse "i1" "c"
                    (Recurse "i2" "d"
                      (Return (OpV "+" "c" "d"))))),
                (Guard [(Match (SymV [] "a") (Lam "arg" "body"))] (Recurse "arg" "c" (Recurse "body" "d" (Return (ClosV "arg" "body")))))]

testeval0 = TestCase(assertEqual "Eval 0 " (True) (compareInterp 10 (SymV [] "a") [eval11] [eval12]))
testeval1 = TestCase(assertEqual "Eval 1 " (True) (compareInterp 10 (SymV [] "a") [eval11] [eval13]))
testeval2 = TestCase(assertEqual "Eval 2 " (True) (compareInterp 10 (SymV [] "a") [eval12] [eval11]))
testeval3 = TestCase(assertEqual "Eval 3 " (True) (compareInterp 10 (SymV [] "a") [eval12] [eval13]))
testeval4 = TestCase(assertEqual "Eval 4 " (True) (compareInterp 10 (SymV [] "a") [eval13] [eval11]))
testeval5 = TestCase(assertEqual "Eval 5 " (True) (compareInterp 10 (SymV [] "a") [eval13] [eval12]))

testeval6 = TestCase(assertEqual "Eval 0 " (True) (compareInterp 10 (SymV [] "a") [eval21] [eval22]))
testeval7 = TestCase(assertEqual "Eval 1 " (True) (compareInterp 10 (SymV [] "a") [eval21] [eval23]))
testeval8 = TestCase(assertEqual "Eval 2 " (True) (compareInterp 10 (SymV [] "a") [eval22] [eval21]))
testeval9 = TestCase(assertEqual "Eval 3 " (True) (compareInterp 10 (SymV [] "a") [eval22] [eval23]))
testeval10 = TestCase(assertEqual "Eval 4 " (True) (compareInterp 10 (SymV [] "a") [eval23] [eval21]))
testeval11 = TestCase(assertEqual "Eval 5 " (True) (compareInterp 10 (SymV [] "a") [eval23] [eval22]))

testeval12 = TestCase(assertEqual "Eval 12 " (True) (compareInterp 10 (SymV [] "a") [eval31] [eval32]))
testeval13 = TestCase(assertEqual "Eval 13 " (True) (compareInterp 10 (SymV [] "a") [eval32] [eval31]))

testeval14 = TestCase(assertEqual "Eval 14 " (False) (compareInterp 10 (SymV [] "a") [eval11] [eval21]))
testeval15 = TestCase(assertEqual "Eval 15 " (False) (compareInterp 10 (SymV [] "a") [eval11] [eval22]))
testeval16 = TestCase(assertEqual "Eval 16 " (False) (compareInterp 10 (SymV [] "a") [eval11] [eval23]))
testeval17 = TestCase(assertEqual "Eval 17 " (False) (compareInterp 10 (SymV [] "a") [eval12] [eval21]))
testeval18 = TestCase(assertEqual "Eval 18 " (False) (compareInterp 10 (SymV [] "a") [eval12] [eval22]))
testeval19 = TestCase(assertEqual "Eval 19 " (False) (compareInterp 10 (SymV [] "a") [eval12] [eval23]))
testeval20 = TestCase(assertEqual "Eval 20 " (False) (compareInterp 10 (SymV [] "a") [eval13] [eval21]))
testeval21 = TestCase(assertEqual "Eval 21 " (False) (compareInterp 10 (SymV [] "a") [eval13] [eval22]))
testeval22 = TestCase(assertEqual "Eval 22 " (False) (compareInterp 10 (SymV [] "a") [eval13] [eval23]))

testeval23 = TestCase(assertEqual "Eval 23 " (False) (compareInterp 1 (SymV [] "a") [eval11] [eval31]))
testeval24 = TestCase(assertEqual "Eval 24 " (False) (compareInterp 1 (SymV [] "a") [eval11] [eval32]))
testeval25 = TestCase(assertEqual "Eval 25 " (False) (compareInterp 1 (SymV [] "a") [eval12] [eval31]))
testeval26 = TestCase(assertEqual "Eval 26 " (False) (compareInterp 1 (SymV [] "a") [eval12] [eval32]))
testeval27 = TestCase(assertEqual "Eval 27 " (False) (compareInterp 1 (SymV [] "a") [eval13] [eval31]))
testeval28 = TestCase(assertEqual "Eval 28 " (False) (compareInterp 1 (SymV [] "a") [eval13] [eval32]))

testeval29 = TestCase(assertEqual "Eval 29 " (False) (compareInterp 1 (SymV [] "a") [eval21] [eval31]))
testeval30 = TestCase(assertEqual "Eval 30 " (False) (compareInterp 1 (SymV [] "a") [eval21] [eval32]))
testeval31 = TestCase(assertEqual "Eval 31 " (False) (compareInterp 1 (SymV [] "a") [eval22] [eval31]))
testeval32 = TestCase(assertEqual "Eval 32 " (False) (compareInterp 1 (SymV [] "a") [eval22] [eval32]))
testeval33 = TestCase(assertEqual "Eval 33 " (False) (compareInterp 1 (SymV [] "a") [eval23] [eval31]))
testeval34 = TestCase(assertEqual "Eval 34 " (False) (compareInterp 1 (SymV [] "a") [eval23] [eval32]))

main :: IO ()
main = do
           counts <- runTestTT ( test [
               testeval1,
               testeval2,
               testeval3,
               testeval4,
               testeval5,
               testeval6,
               testeval7,
               testeval8,
               testeval9,
               testeval10,
               testeval11,
               testeval12,
               testeval13,
               testeval14,
               testeval15,
               testeval16,
               testeval17,
               testeval18,
               testeval19,
               testeval20,
               testeval21,
               testeval22,
               testeval23,
               testeval24,
               testeval25,
               testeval26,
               testeval27,
               testeval28,
               testeval29,
               testeval30,
               testeval31,
               testeval32,
               testeval33,
               testeval34
             ])
           if (errors counts + failures counts == 0)
               then exitSuccess
               else exitFailure
