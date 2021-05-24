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
-- 1. class Arguable a 
-- 2. data Proposition' = forall a . Arguable a => Proposition' {title :: Name , content :: a } deriving Eq 

{--}
data Proposition =  Proposition {title :: Name , content :: String } deriving Eq 

type Propositions = [Proposition]

type P'preference = Map.HashMap Proposition Int  

-- | TODO:
-- Is this pending here make sense ?
data TopRule = Defeasible | Strict | Undecided deriving Eq 

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

type R'preference = Map.HashMap Rule Int  

{--}
data Context = Context {rules :: Rules , rulePrefer :: R'preference, propPrefer :: P'preference}

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

data Expandability = Complete | Pending | Expandable 