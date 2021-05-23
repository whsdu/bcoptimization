{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Partial.Argument where 

import Prelude hiding (head)
import Control.Monad(guard)
import Data.Kind (Type)
import GHC.TypeLits ( Symbol )
import qualified Data.HashMap.Strict as Map 

type Name = String 

-- | TODO
-- update Proposition's content to include more datatype . 
-- class Arguable a 
-- data Proposition' = forall a . Arguable a => Proposition' {title :: Name , content :: a } deriving Eq 

{--}
data Proposition =  Fact | Proposition {title :: Name , content :: String } deriving Eq 

type Propositions = [Proposition]

{-
could be abstracted as env
now using classical negative
-}
negation :: Proposition -> Proposition 
negation = undefined 

isNegate :: Proposition -> Proposition -> Bool 
isNegate = undefined 

type PropPrefer = Map.HashMap Proposition Int  

{--}
data TopRule = Defeasible | Strict | Pending deriving Eq 


data Rule = Rule {name :: Name , head :: Proposition , imp :: TopRule , body :: Propositions} 

-- | TODO: in this case two rules must have different head or imp or body
-- otherwise should throw exception when read in file. 
-- how to guarantee this uniqueness . 
-- maybe use set for Rules ? 
instance Eq Rule where 
    r1 == r2 = 
        head r1 == head r2 
        &&
        imp r1 == imp r2 
        &&
        body r1 == body r2

type Rules = [Rule]

type RulePrefer = Map.HashMap Rule Int  

{--}
data Context = Context {rules :: Rules , rulePrefer :: RulePrefer, propPrefer :: PropPrefer}

{--}
data Argument  where 
    L'Argument :: Proposition -> Argument 
    P'Argument :: Proposition -> TopRule -> [Argument] -> Argument 

instance Eq Argument where 
    (L'Argument a) == (L'Argument b) = a == b
    (P'Argument a impa as) == (P'Argument b impb bs) = 
        a == b 
        &&
        impa == impb
        && 
        length [ a' | a' <- as, a' `elem` bs] == length as 
        &&
        length [ b' | b' <- bs, b' `elem` as] == length bs 

conC :: Argument  -> Proposition  
conC (L'Argument p) = p 
conC (P'Argument c _ _ ) = c

sub :: Argument -> [Argument]
sub a@(L'Argument _) = [a]
sub (P'Argument _ _ as) = concat $ sub <$> as 

-- | TODO: the name of the rule should be check from the Context 
topRule :: Argument -> Rule
topRule (L'Argument p) = Rule  "" p Pending []
topRule (P'Argument p tr subs) = Rule "" p tr $ conC <$> subs 

getRuleName :: Context -> Rule -> Maybe Name 
getRuleName c r = 
        if null rls' then Nothing 
        else 
            Just $ name $ rls'!!0
        where 
            rls = rules c 
            rls' = do 
                    r' <- rls
                    guard $ r' == r
                    pure r'

-- | TODO: what should this return will be more efficient ? 
comp :: Context -> Argument -> [Proposition]
comp context arg = 
    let 
        args = conC <$> sub arg 
        rls = rules context
        rs = and [ a `elem` (head <$> rls) | a<- args ]
    in undefined 


data Binary = Binary 
                { check :: Context -> Argument -> Argument -> Bool
                , construct :: Context -> Argument -> [Argument]
                }

data RelationalTree  = 
                    Disard () 
                  | Alone Argument 
                  | Expand (Argument , RelationalTree)

type Goal = RelationalTree -> Int 

type Actions = [Binary]
type Goals = [Goal]

-- | define negation of proposition 

rebut :: Binary 
rebut = Binary check' construct'
    where 
        check' _ a b = 
            let 
                aConc = conC a 
                bConc = conC b 
            in undefined 

        construct' context a = 
            let 
                rls = rules context 
                aConc = conC a
                aRebut = Proposition (title aConc) ("!"++content aConc)
            in [ L'Argument aRebut | aRebut `elem` (head <$> rls)]

undercut:: Binary
undercut = Binary check' construct' 
    where 
        check' _ a b = undefined 
        construct' = undefined 

warranted :: Goal 
warranted = undefined 

unwarranted :: Goal 
unwarranted = undefined 




{- Basic types -}
{- Information in the input file-}

-- | this is actually the set of Defeasible Rules 
-- data Record = Language {name :: String , head :: String, imp :: Imp, body :: String}

-- type Records = [Record]
-- -- | PreferenceMap : Each element of L could associated with some kind of preference  
-- type PreferenceMap a = Map.HashMap Record a 

-- {-run-time environment -}
-- data Env' a = Env' {records :: Records, preference' :: PreferenceMap a}

-- class Has' field env where 
--     obtain' :: env -> field 

-- instance Has' Records (Env' a) where obtain' = records
-- instance Has' (PreferenceMap a) (Env' a) where obtain' = preference'




-- comp :: Argument a -> L 
-- comp (L'Argument _) = Atom 
-- comp (P'Argument _ _ sub) = 

-- | Rules that conclude certain set of proposition 
-- type Expansion = [L]

-- data Argument where 
--  L'Argument :: Atom 
--  P'Argument :: Argument Atom -> [Argument a] -> Argument Rule 


-- | In this case the argument refers to argument in all three status
-- data Argument  = Argument {partial :: [L], comp ::[L]}

-- expandable :: 


-- data RelationalTree  = 
--                     Disard () 
--                   | Alone Argument 
--                   | Expand (Argument , RelationalTree)

-- is this enough ? 
-- should include constrain that contains AT and prefernce of AT 
-- type Goal = RelationalTree -> Bool 

-- class Binary (s :: Symbol) where 
--     p :: Argument -> Argument -> Bool 
--     c :: Argument -> Maybe Argument

-- Type level constrain related to information beyond semantics (exact relation instance)

-- This is a interesting idea
-- instance Binary "undercut" where 
--     p _ _ = True 
--     c = undefined 

-- f ::  Argument -> Argument -> Bool 
-- f = p @"undercut" 


-- | todo , see how typeclass use together with GADTs


{-
0. not intuitive enough
1. minimal usable application 
2. handle real world dataset 
3. customizable plan 
-}

{- Problem of other domains: 
1. to abstract higher order relation under context 
2. to provide compile time constraint
-}

-- class Binary (s :: Symbol) where 
--     p :: Context -> Argument -> Argument -> Bool 
--     c :: Context -> Argument -> Maybe Argument 
-- 
-- instance Binary "undercut" where 
--     p env a1 a2 = undefined 
--     c context a = undefined 
-- 
-- 
-- instance Binary "rebut" where 
--     p env a1 a2 = undefined 
--     c context a = undefined 