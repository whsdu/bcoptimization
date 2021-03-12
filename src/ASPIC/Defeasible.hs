{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module ASPIC.Defeasible
    ( Literal (..)
    , Language 
    , Path 
    , Argument
    , Imp(..)
    , Name
    , Rules(..)
    , LogicLanguage(..)
    , Board(..)
    , SearchRecord
    , SearchRecords
    , PathRecord
    , PathRecords
    , Defeater(..)
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
    Atom :: (Show a, Eq a) =>  Name -> a -> Literal a
    Rule ::  (Show a, Eq a) => Name -> [Literal a] -> Imp -> Literal a-> Literal a 

instance (Show a) => Show (Literal a) where
    show (Rule n b i h) = n ++ ": " ++ bs ++ im ++ head
        where
            bs = unwords $ name <$> b
            im = show i
            head = name h
    show (Atom n _) = n

instance (Eq a) => Eq (Literal a) where
    (Atom n1 a1) == (Atom n2 a2) = (n1 == n2) && (a1 ==a2) 
    (Rule n1 body1 imp1 h1) == (Rule n2 body2 imp2 h2) = 
        let 
            nameEq  = n1 == n2 
            bodyEq  = (length body1 == length body2)
            impEq   = imp1 == imp2 
            headEq  = h1 == h2 
        in nameEq && bodyEq && impEq && headEq 


-- instance Applicative Literal where 
--     pure = Atom ""
--     (Atom _ f) <*> (Atom n a) = Atom n (f a)
--     f <*> (Rule _ _ _ c) = f <*> c 


-- A set of rules 
-- [r1,r2,r3,r4]
type Language a = [Literal a] 

newtype Rules a = Rules {getRules :: Language a }
newtype LogicLanguage a= LogicLanguage {getLogicLanguage :: Language a}

instance (Show a) => Show (Rules a) where 
    show  = show . getRules

instance (Show a) => Show (LogicLanguage a) where 
    show  = show . getLogicLanguage

-- A list of sets of rules 
-- [[r1,r2],[r3,r4]]
-- 1. Path that satisfy path properties 
type Path a = [Language a] 

-- A list of Path
-- 1. Argument 
-- 2. In complete- argument 
type Argument a = [Path a]

{-
Following types:
 'Base', 'DefeaterStatus', 'Defater' , 
are used for BCOptimization algorithm. 
-}

-- data DefeaterStatus = Warranted Argument | Unwarranted Argument | Pending Argument 

data Defeater = forall a .(Show a) => SW (Argument a)| forall a. (Show a) => Warranted (Path a, Defeater) | forall a. (Show a ) => Unwarranted [(Path a,Defeater)] 


type SearchRecord a = (Path a,Defeater) 
type SearchRecords a = [SearchRecord a]

type PathRecord a = (Path a,Argument a) 
type PathRecords a = [PathRecord a]

data Board = forall a . (Show a ) => Board {lucky :: SearchRecords a, waiting :: PathRecords a, futile :: SearchRecords a, seen :: Language a}

instance Show Defeater where 
    show (SW p) = show p 
    show (Warranted sub) = 
        "Warranted Node: " ++ "\n" ++ showSingleTree sub ""
    show (Unwarranted sub) = 
        "Unwarranted Node: " ++ "\n" ++ showSubTree sub

showSubTree :: forall a . (Show a) => [(Path a, Defeater)] -> String 
showSubTree = foldr showSingleTree "" 
showSingleTree :: forall a. (Show a) =>  (Path a,Defeater) -> String -> String 
showSingleTree (p,d) s = 
    let
        content =  
            "Path:" ++ show p ++ "\n" ++ 
            "defeater" ++ show d 
    in content ++ s 

instance Show Board where 
    show Board{..} = 
        "LUCKY: " ++ show lucky ++ "/n" ++
        "WAITING: " ++ show waiting ++ "/n" ++ 
        "FUTILE: " ++ show futile ++ "/n" ++ 
        "SEEN: " ++ show seen 


{-
Auxiliaies types and functions below
-}
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

-- type LiteralMap = forall a . Map.HashMap Name (Literal a) 
-- newtype StrictRules = StrictRules {getStrictRules :: Language}
-- newtype DefeasibleRules = DefeasibleRules {getDefeasibleRules :: Language}

-- type PreferenceMap = Map.HashMap Name Int 
-- newtype RdPrefMap = RdPrefMap {getRdPrefMap :: Map.HashMap Name Int}
-- newtype KnwlPrefMap = KnwlPrefMap { getKnwlPrefMap :: Map.HashMap Name Int}

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



