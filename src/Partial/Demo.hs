module Partial.Demo where 

import qualified Data.HashMap.Strict as Map

import qualified Partial.Argument as A 
import qualified Partial.Properties as Prop
import qualified Partial.Planner as Planner


{-
1. Define many binaries
-}

rebut :: Planner.BinaryRelation 
rebut = Planner.Binary check' construct'
    where 
        check' a b = 
            let 
                aConc = Prop.conC a 
                bConc = Prop.conC b 
            in undefined 

        construct' context a = 
            let 
                rls = A.rules context 
                aConc = Prop.conC a
                aRebut = A.Proposition (A.title aConc) ("!"++A.content aConc)
            in [ A.L'Argument aRebut | aRebut `elem` (A.head <$> rls)]

undercut:: Planner.BinaryRelation 
undercut = Planner.Binary check' construct' 
    where 
        check' a b = undefined 
        construct' = undefined 

expand :: Planner.BinaryRelation 
expand = undefined 

defeated :: Planner.BinaryRelation 
defeated = undefined  

actions :: Planner.Actions
actions = [expand, defeated]

{-
2. Define Goal
-}
{-
1. defeated action is necessary. 
2. only 
-}
warranted :: Planner.Goal 
warranted (Planner.Discard _)  = Nothing 
warranted (Planner.Alone a) 
    | null (Prop.comp a) = Just 0
    | otherwise  = Just 1
warranted (Planner.Expand a rts)
    | (Planner.Discard _) `elem` rts = Nothing 
    | 
    let 
        r1 = and $ (Planner.check defeated) a <$> 

unwarranted :: Planner.Goal 
unwarranted = undefined 

{-
3. define a planner
-- Search Strategies includes:
    - Best first 
    - 
tasks: 
1.compose a example first 
2. 
-}

aStar :: Planner.Planner 
aStar queryA actions goals = 
    let fringe = []
        
-- | not necessarily all goals reaches Nothing 
--  how to deal with this ? 
-- set fringe a set of relational trees. 
-- apply heuristic function on fringe and select the lowest value 

{-
4. feed to planner see what's going on
-}

query :: A.Argument 
query = undefined 



schedule :: Planner.Aims
schedule = Map.fromList [(1, warranted),(2,unwarranted)]

queryResult :: Maybe (Int, Planner.RelationalTree) 
queryResult =  aStar query actions schedule

{- The difference between this and traditional planning
1.  traditional planning : find the path 
    this : compute  the states. 
2. traditional , shorted path ( a sequence of states) 
    this: one state. (heuristic 的意义就被消减了)
    for example:  a state is possible computed with less computation
    but has seemingly large heuristic thus be omitted. (multi goals)
we do not interested in the path, instead we interested in the goal states.

the actual computation could be really expansive, focus on the plan . 
this foucs on the result with less expansive computation. 
-}

{-
traditional good heuristic : reduce the number of states that need to be evaluated. 
this : pretty much the same. 

this: we care about computation complexity related to search the path (in traditional way).

maybe drop some action constrain ? but the state is not exist yet !
-}

