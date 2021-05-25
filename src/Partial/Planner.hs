module Partial.Planner where 

import qualified Data.HashMap.Strict as Map

import qualified Partial.Argument as A 
import qualified Partial.Properties as P 

{-
1. Binary relations == Actions
-}
data BinaryRelation = Binary 
                { check ::  A.Argument -> A.Argument -> Bool
                , construct :: A.Context -> A.Argument -> [A.Argument]
                }

-- | these two definitions are not necessary. 
type Actions = [BinaryRelation]
-- type Actions = Map.HashMap Int BinaryRelation

{-
2. Represents the State of actions 
-}
-- | TODO: this might need to be updated !
-- This is different from the paper. 
-- implies only one action at a time
-- may be this ? : | Expand A.Argument  [RelationalTree]
-- Expand mean some kind of relation. 
data RelationalTree  = 
                    Discard () 
                  | Alone A.Argument 
                  | Expand A.Argument  [RelationalTree]

instance Eq RelationalTree where 
  Discard _ == Discard _ = True 
  Alone a1 == Alone a2 = a1 == a2 
  (Expand a aTree) == (Expand b bTree) = 
            a == b && aTree == bTree 
  Discard _ == Alone _ = False 
  Discard _ == Expand _ _ = False 
  Alone _ == Discard _ = False 
  Alone _ == Expand _ _ = False 
  Expand _ _ == Discard _ = False 
  Expand _ _ == Alone _ = False 

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
type Aims = Map.HashMap Int Goal
type Planner = A.Context -> A.Argument -> Actions -> Aims -> Maybe (Int, RelationalTree)


{-
if the goal interested in the action, then it must be included sooner or later
-}