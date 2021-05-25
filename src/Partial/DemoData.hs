module Partial.DemoData where 

import qualified Partial.Argument as A 
-- | Basic process 
{- load data set from file -}
--  1. file contains rules
--  2. file parser convert rules into two sets: Propositions & Rules 

{-
r1: b,c -> a 
r2: d => b 
r3: [] => c 
r4: [] -> d 
-}

a :: A.Proposition
b :: A.Proposition
c :: A.Proposition
d :: A.Proposition

a = A.Proposition "a" "a" 
b = A.Proposition "b" "b"
c = A.Proposition "c" "c" 
d = A.Proposition "d" "d" 

r1 :: A.Rule 
r2 :: A.Rule
r3 :: A.Rule
r4 :: A.Rule

r1 = A.Rule "r1" a A.Strict [b,c]
r2 = A.Rule "r2" b A.Defeasible [d]
r3 = A.Rule "r3" c A.Defeasible  []
r4 = A.Rule "r4" d A.Strict [] 

{-
r5:e->!r2
r6:f=>e
r7:[]->f
-}
e :: A.Proposition
f :: A.Proposition
ngR2 :: A.Proposition

e = A.Proposition "e" "e" 
f = A.Proposition "f" "f"
ngR2 = A.Proposition "rule" "!r2" 

r5 :: A.Rule 
r6 :: A.Rule 
r7 :: A.Rule 

r5 = A.Rule "r5" ngR2 A.Strict  [e]
r6 = A.Rule "r6" e A.Defeasible  [f]
r7 = A.Rule "r7" f A.Strict []

{-
r8:g->!b
r9:h=>g
r10:[]=>h
-}
g :: A.Proposition
h :: A.Proposition
ngB :: A.Proposition 

g = A.Proposition "g" "g" 
h = A.Proposition "h" "h"
ngB = A.Proposition "!b" "!b"

r8 :: A.Rule 
r9 :: A.Rule 
r10 :: A.Rule 

r8 = A.Rule "r8" ngB A.Strict [g]
r9 = A.Rule "r9" g A.Defeasible  [h]
r10 = A.Rule "r10" h A.Defeasible  []

{-
r11:i->!c
r12:j=>i
r13:[]=>j
-}
i :: A.Proposition
j :: A.Proposition

i = A.Proposition "i" "i" 
j = A.Proposition "j" "j"
ngC = A.Proposition "!c" "!c"

r11 :: A.Rule 
r12 :: A.Rule 
r13 :: A.Rule 

r11 = A.Rule "r11" ngC A.Strict [i]
r12 = A.Rule "r12" i A.Defeasible [j]
r13 = A.Rule "r13" j A.Defeasible  []

{--}

demoRules :: A.Rules 
demoRules = [ r1, r2, r3, r4, r5, r6, r7, r9, r10, r11, r12, r13]

demoPropositions :: A.Propositions 
demoPropositions = [a,b,c,d,e,f,g,h,i,j,ngB,ngC,ngR2]

{- Notes: 
1. Use (Proposition , Rule , Argument)  vs Use Literal 
implementation: negation is either defined on Literal or defined on Argument. 
Reasoning: 
    In the related paper such as \tutorial, negation is defined on logic language l 

-}