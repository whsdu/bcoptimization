{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module ASPIC.Defeasible
    ( Literal (..)
    , Language 
    , Path 
    , Argument
    , Imp(..)
    , Name
    , Board(..)
    , SearchRecord
    , SearchRecords
    , PathRecord
    , PathRecords
    , Defeater(..)

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
    , branchDef
    , PreferenceMap
    , LiteralMap
    , Rules(..)
    , LogicLanguage(..)
    ) where 
    

import qualified Data.HashMap.Strict as Map
import qualified GHC.List as GHC (head)

-- | Literal is defined recursively because body and conclusion(head) or rules could also be rule itself.
-- `Rule` is constructor of `Ordinary Premises`, `Axiom Premises`, `Strict Rules` & `Defeasible Rules`. 
-- `Atom` is constructor of conclusion other than above `Premises` or `Rules`.   
-- `n` introduced in paper maps a rule to a literal, it is not necessary here when Literal is defined recursively like this.


-- data Statement a = Statement a | Deduction

-- TODO: 
{-
    1. actually, Atom could also be represented by Rule, with 'Imp` being 'N', this maybe over engineered 
    If it is possible maybe use type programming to handle this ?
-}

data Literal a where
    Atom :: (Show a, Eq a) =>  Name -> a -> Literal a
    Rule ::  (Show a, Eq a) => Name -> [Literal a] -> Imp -> Literal a-> Literal a 

-- | TODO:
{-
    1. 'a' should also be included in the Show instance.
    2. The same with other instance.  
-}
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
    (Atom _ _) == Rule{} = False
    Rule{} == Atom{} = False 


-- instance Applicative Literal where 
--     pure = Atom ""
--     (Atom _ f) <*> (Atom n a) = Atom n (f a)
--     f <*> (Rule _ _ _ c) = f <*> c 

{-Language-}
-- A set of rules 
-- [r1,r2,r3,r4]
type Language a = [Literal a] 

{-
Wrapper of Language so that we could tell the difference between Rules and Language.
-}
newtype Rules a = Rules {getRules :: Language a }
newtype LogicLanguage a = LogicLanguage {getLogicLanguage :: Language a}

instance (Show a) => Show (Rules a) where 
    show  = show . getRules

instance (Show a) => Show (LogicLanguage a) where 
    show  = show . getLogicLanguage

type LiteralMap a=  Map.HashMap Name (Literal a) 
type PreferenceMap = Map.HashMap Name Int 

{-Path-}
-- A list of sets of rules 
-- [[r1,r2],[r3,r4]]
-- 1. Path that satisfy path properties 
type Path a = [Language a] 

{-Argument-}
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

data Defeater a = 
                  (Show a) => SW (Argument a)
                | (Show a) => Warranted (Path a, Defeater a)
                | (Show a) => Unwarranted [(Path a, Defeater a)] 
                | NoDefeater

type SearchRecord a = (Path a,Defeater a) 
type SearchRecords a = [SearchRecord a]

type PathRecord a = (Path a,Argument a) 
type PathRecords a = [PathRecord a]

-- | Lucky contains Either SearchRecords with NoDefeaters or SearchRecord with Unwarranted defeaters 
-- futile contains SearchRecords with Warranted defeaters only. 
-- TODO: this should be a dependent type. 
data Board a = Board {lucky :: SearchRecords a, waiting :: PathRecords a, futile :: SearchRecords a, seen :: Language a}

instance (Show a) => Show (Defeater a) where 
    show (SW p) = show p 
    show NoDefeater = "NO-Defeater"
    show (Warranted sub) = 
        "Warranted Node: " ++ "\n" ++ showSingleTree sub ""
    show (Unwarranted sub) = 
        "Unwarranted Node: " ++ "\n" ++ showSubTree sub

showSubTree ::  (Show a) => [(Path a, Defeater a)] -> String 
showSubTree = foldr showSingleTree "" 
showSingleTree :: (Show a) =>  (Path a, Defeater a) -> String -> String 
showSingleTree (p,d) s = 
    let
        content =  
            "Path:" ++ show p ++ "\n" ++ 
            "defeater" ++ show d 
    in content ++ s 

instance (Show a) => Show (Board a) where 
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

branchDef :: forall a.(Eq a) => Path a-> Language a -> Path a
branchDef mp [] = []
branchDef mp lang = 
    let 
        rules = concat mp 
        currentLevel = [ r |l <- lang, r <- rules, conC r == l] 
        nextLevelPros = concat $ body <$> currentLevel
    in currentLevel : branchDef mp nextLevelPros
