module Partial.Planner where 

import qualified Data.HashMap.Strict as Map

import qualified Partial.Argument as A 
import qualified Partial.Properties as P 

{-
1. Binary relations == Actions
-}
data BinaryRelation = Binary 
                { check :: A.Context -> A.Argument -> A.Argument -> Bool
                , construct :: A.Context -> A.Argument -> [A.Argument]
                }
type Actions = [BinaryRelation]

{-
2. Represents the State of actions 
-}
-- | TODO: this might need to be updated !
-- This is different from the paper. 
-- implies only one action at a time
-- may be this ? : | Expand A.Argument  [RelationalTree]
-- Expand mean some kind of relation. 
data RelationalTree  = 
                    Disard () 
                  | Alone A.Argument 
                  | Expand A.Argument  RelationalTree

{-
3. Heuristic function 
under estimate the distance ==> admissible 
-}
type Goal = RelationalTree -> Maybe Int 

{-
3.1 
    Nothing = Infinite 
    Just 0  = Goal 
    Just a  = a >=0  is Heuristic value , the smaller the better. 
-}
type Goals = [Goal]

{-
4. Plan
-}
type Schedule = Map.HashMap Int Goal
type Planner = A.Argument -> Actions -> Schedule -> Maybe (Int, RelationalTree)
