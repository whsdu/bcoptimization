{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE DatatypeContexts #-}

module ASPIC.AS
    ( Literal (..)
    , Language 
    , Path 
    , Argument
    , Imp(..)
    , Name
    , Rules(..)
    , LogicLanguage(..)
    -- , LiteralMap
    -- , StrictRules (..)
    -- , DefeasibleRules(..)
    -- , PreferenceMap
    -- , RdPrefMap(..)
    -- , KnwlPrefMap(..)
    , name
    , body
    , imp
    , conC
    , statement
    ) where 
    

import qualified Data.HashMap.Strict as Map
import qualified GHC.List as GHC (head)

-- | Literal is defined recursively because body and conclusion(head) or rules could also be rule itself.
-- `Rule` is constructor of `Ordinary Premises`, `Axiom Premises`, `Strict Rules` & `Defeasible Rules`. 
-- `Atom` is constructor of conclusion other than above `Premises` or `Rules`.   
-- `n` introduced in paper maps a rule to a literal, it is not necessary here when Literal is defined recursively like this.
-- TODO: actually, Atom could also be represented by Rule, with 'Imp` being 'N', this maybe over engineered 
-- If it is possible maybe use type programming to handle this ?

-- data Statement a = Statement a | Deduction

data Literal a where
    Atom ::  Name -> a -> Literal a
    Rule ::  Name -> [Literal a] -> Imp -> Literal a-> Literal a deriving(Functor, Monad)

instance Show (Literal a) where
    show (Rule n b i h) = n ++ ": " ++ bs ++ im ++ head
        where
            bs = unwords $ name <$> b
            im = show i
            head = name h
    show (Atom n _) = n

instance Applicative Literal where 
    pure = Atom ""
    (Atom _ f) <*> (Atom n a) = Atom n (f a)
    f <*> (Rule _ _ _ c) = f <*> c 

-- instance Eq (Literal a) where 
    -- l1 == l2 = name l1 == name l2 

-- | Preference needs to be redefined 

-- | `L` language is a set of `Literal`

-- A set of rules 
-- [r1,r2,r3,r4]
type Language  = forall a . [Literal a]

newtype Rules = Rules {getRules :: Language}
newtype LogicLanguage = LogicLanguage {getLogicLanguage :: Language}

instance Show Rules where 
    show  = show . getRules

instance Show LogicLanguage where 
    show  = show . getLogicLanguage

-- A list of sets of rules 
-- [[r1,r2],[r3,r4]]
-- 1. Path that satisfy path properties 
type Path = forall a . [[Literal a]]

-- A list of Path
-- 1. Argument 
-- 2. In complete- argument 
type Argument = forall a . [[[Literal a]]] 

type Name = String 

data Imp = S | D | N

instance Show Imp where 
    show S = "->"
    show D = "=>"
    show N = " "

instance Eq Imp where 
    (==) S S = True 
    (==) D D = True
    (==) N N = True 
    (==) _ _ = False

-- | LanguageMap is a dictionary used to query Literal with given name
type LiteralMap = forall a . Map.HashMap Name (Literal a) 
newtype StrictRules = StrictRules {getStrictRules :: Language}
newtype DefeasibleRules = DefeasibleRules {getDefeasibleRules :: Language}

type PreferenceMap = Map.HashMap Name Int 
newtype RdPrefMap = RdPrefMap {getRdPrefMap :: Map.HashMap Name Int}
newtype KnwlPrefMap = KnwlPrefMap { getKnwlPrefMap :: Map.HashMap Name Int}

-- | name of an instantiation of type `Literal`: it plays two rules:
-- 1. To be used to guarantee the uniqueness of a `Literal`.
-- 2. To be used to defined negation with simple `!`.
name :: Literal a-> Name
name (Rule n _ _ _) = n
name (Atom n _ )       = n

-- | Body Imp Conc
-- Get body of a rule
body  :: Literal a -> [Literal a]
body (Rule _ b _ _) = b
body (Atom _ _)       = []

-- | Body Imp Conc
-- Get Imp or a rule 
imp :: Literal a-> Imp
imp (Rule _ _ i _) = i
imp (Atom _ _ )       = N

-- | Body Imp Conc
-- Get conclusion (head) of a rule 
conC :: Literal a -> Literal a
conC (Rule _ _ _ h) = h
conC a@(Atom _ _)     = a

statement :: Literal a -> a 
statement (Rule _ _ _ h) = statement h 
statement (Atom _ a) = a 



