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



{-
2. Define Goal
-}
{- these instance of goals, actions are tightly coupled
1. defeated action is compulsory. 
2. only 
-}
warranted :: Planner.Goal 
warranted (Planner.Discard _)  = Nothing 
warranted (Planner.Alone a) 
    | null (Prop.comp a) = Just 0
    | otherwise  = Just 1
warranted (Planner.Expand a rts) = undefined 
    -- | (Planner.Discard _) `elem` rts = Nothing 
    -- | otherwise  = undefined 


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

action has two dimensions 
    1. optionality: optional / compulsory 
    2. replaceable :     replaceable/ not replaceable 

other dimension maybe necessary

    replaceable, choice one result at a time. 
    not replaceable, choice all ? 
-}

aStar :: Planner.Planner 
aStar context queryA actions goals = 
    let 
        arguCs = _constructActions context actions queryA
        tmpContext = _updateContext context arguCs 
        arguEs = _checkingActions context actions queryA
        fringe = _relationalTreeCons queryA (arguCs ++ arguEs)
        vs = _evaluate goals fringe 
    in undefined 

_relationalTreeCons :: A.Argument -> [A.Argument] -> [Planner.RelationalTree]
_relationalTreeCons = undefined 

_checkingActions :: A.Context -> Planner.Actions -> A.Argument  -> [A.Argument]
_checkingActions context actions queryA = 
    let 
        cheks = Planner.check <$> actions
        aguSpc = A.space context 
    in  auC aguSpc cheks queryA [] 
    where 
        auC space [c] queryA acc = filter (c queryA) space  ++ acc 
        auC space (c:cs) queryA acc = auC space cs queryA (filter (c queryA) space  ++ acc)

-- ｜ TODO: implementation order does not matter in this case. 
_constructActions :: A.Context -> Planner.Actions -> A.Argument -> [A.Argument]
_constructActions context actions queryA  = 
    let 
        cons = Planner.construct <$> actions 
    in  p context cons [queryA]
    where 
        p context [c] args = concat $ c context <$> args 
        p context (c:cs) args = p context cs $ concat $ c context <$> args 

_updateContext :: A.Context -> [A.Argument] -> A.Context
_updateContext context argus = context{A.space = A.space context ++ argus}

_evaluate :: Planner.Aims -> [Planner.RelationalTree ] -> Map.HashMap Int [Maybe Int]
_evaluate aims rts = 
    let 
        ms = Map.toList aims 
        p = do 
            (n,a) <- ms 
            pure  (n, a <$> rts)
    in Map.fromList p

{-
initialize the parameters
-}

query :: A.Argument 
query = undefined 

context :: A.Context 
context = undefined 

actions :: Planner.Actions
actions = [expand, defeated]

schedule :: Planner.Aims
schedule = Map.fromList [(1, warranted),(2,unwarranted)]

queryResult :: Maybe (Int, Planner.RelationalTree) 
queryResult =  aStar context query actions schedule

{-
if we are trying to provide a Domain Specific framework. 

it is always the case that we need to abstract the syntax that can factorize the semantic space. 

the problem is the semantic space is not as rich as we expected, thus the abstraction would bring unnecessary overload. 
(this is why if feels a little bit overkill)

or 

it could be possible that because we do not have a well designed abstraction layer of the syntax , so it stopped us from 
exploring possibly more useful semantics. 
-}


{-
keep track the rule being used 
and 
reuse existing state 
-}