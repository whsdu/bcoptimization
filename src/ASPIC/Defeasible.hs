{-|
This module contains two groups of definitions .

Group one refers to components of a Defeasible Theory that can be used to build an Argumentation System.

- Types: Literal, Language, Argument, Argument Group, etc.
- Functions: name, body, imp, conC, etc.

Group two refers to components that critical for backward chaining algorithm.

- Defeater, Board, SearchRecord(s), PathRecord(s), PreferenceMap,LiteralMap,etc.

TODO: 'ASPIC.Defeasible' and 'ASPIC.Abstraction' are core of this library.
Others, such as Run, Default, Ordering, Parser, Algorithm, should be the implementation of this library.
-}

{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE Safe                 #-}
{-# LANGUAGE TypeSynonymInstances #-}


module ASPIC.Defeasible
    (
    -- * Defeasible Theory
    -- ** Data Types
      Literal (..)
    , Imp(..)
    , Name
    , Board(..)
    , Language
    -- ** Auxiliary Functions
    , name
    , body
    , imp
    , conC
    -- * Backward Chaining
    -- ** Data Types
    , Argument
    , ArgumentGroup
    , Defeater(..)
    , SearchRecord
    , SearchRecords
    , PathRecord
    , PathRecords
    -- ** Auxiliary Functions
    , statement
    , branchDef
    -- ** Run-time Environment Information
    , PreferenceMap
    , LiteralMap
    , Rules(..)
    , LogicLanguage(..)
    ) where


import qualified Data.HashMap.Strict as Map
import qualified GHC.List            as GHC (head)

-- | Literal is defined recursively because body and head(conclusion) or rules could also be a rule.
--
-- 'Rule' is constructor of __Ordinary Premises__, __Axiom Premises__, __Strict Rules__ & __Defeasible Rules__.
--
-- 'Atom' is constructor of __Proposition__ other than above 'Premises' or 'Rules'.
--
-- Function __'n'__ introduced in <https://content.iospress.com/articles/argument-and-computation/869766 paper>
-- maps a rule to a literal, it is not necessary here when Literal is defined recursively like this.

data Literal a where
    Atom :: (Show a, Eq a) =>  Name -> a -> Literal a
    Rule :: (Show a, Eq a) => Name -> [Literal a] -> Imp -> Literal a-> Literal a
{- TODO:
    1. Atom could also be represented by Rule, with 'Imp` being 'N', this maybe over engineered
    If it is possible maybe use type programming to handle this ?
-}

-- | Logic Language is a set of 'Literal's.
type Language a = [Literal a]

-- | Wrapper of 'Language' to represents Rule 'Literal's.
newtype Rules a = Rules {getRules :: Language a }

-- | Wrapper of 'Language' to represents all 'Language'.
newtype LogicLanguage a = LogicLanguage {getLogicLanguage :: Language a}
{-
TODO: Could be replaced with 'Language' directly maybe?
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



instance (Show a) => Show (Rules a) where
    show  = show . getRules

instance (Show a) => Show (LogicLanguage a) where
    show  = show . getLogicLanguage


-- | Dictionary being used to get literal given certain name
--
-- e.g. Getting a proposition of name "q1"
type LiteralMap a=  Map.HashMap Name (Literal a)

-- | Dictionary being used to store pre-defined priority orderings
--
-- Currently the priority ordering value is of type 'Int' and is associated with 'Rule's only.
type PreferenceMap = Map.HashMap Name Int

-- | A Path is a list of sets of rules, e.g.
-- [[r1,r2],[r3,r4]]
--
-- A Path satisfies all 5 path properties
type Argument a = [Language a]
{-
TODO: This will be renamed as __Argument__.
-}

-- | An Argument is a set of Argument
type ArgumentGroup a = [Argument a]
{-
TODO: Will be renamed as __ArgumentGroup__.
-}

-- | DefeatTree
--
-- Consists of an incomplete-argument and a group of argument that defeat this argument.
data Defeater a =
                  (Show a) => SW (ArgumentGroup a)
                | (Show a) => Warranted (Argument a, Defeater a)
                | (Show a) => Unwarranted [(Argument a, Defeater a)]
                | NoDefeater
{-
TODO: Will be renamed as __DefeatTree__.
-}

-- | Consists of an incomplete-argument and a group of argument that defeat this argument.
type SearchRecord a = (Argument a,Defeater a)
type SearchRecords a = [SearchRecord a]

-- | Consists of an incomplete-argument and a group of argument that attack this argument.
type PathRecord a = (Argument a,ArgumentGroup a)
type PathRecords a = [PathRecord a]

-- | lucky contains Either SearchRecords with NoDefeaters or SearchRecord with Unwarranted defeaters
--
-- futile contains SearchRecords with (Self)Warranted defeaters.
--
data Board a = Board {lucky :: SearchRecords a, waiting :: PathRecords a, futile :: SearchRecords a, seen :: Language a}
{-
TODO: Could this be rewritten using type level programming (some kind of DSL)?
-}

instance (Show a) => Show (Defeater a) where
    show (SW p) = "Self-Warranted : " ++ show p
    show NoDefeater = "NO-Defeater"
    show (Warranted sub) =
        "Warranted : " ++ showSingleTree sub ""
    show (Unwarranted sub) =
        "Unwarranted : " ++ showSubTree sub

showSubTree ::  (Show a) => [(Argument a, Defeater a)] -> String
showSubTree = foldr showSingleTree ""
showSingleTree :: (Show a) =>  (Argument a, Defeater a) -> String -> String
showSingleTree (p,d) s =
    let
        content =
            "Argument: " ++ show p ++
            "defeater: " ++ show d
    in content ++ s

instance (Show a) => Show (Board a) where
    show Board{..} =
        "LUCKY: " ++ show lucky ++ "\n" ++
        "WAITING: " ++ show waiting ++ "\n" ++
        "FUTILE: " ++ show futile ++ "\n" ++
        "SEEN: " ++ show seen ++ "\n"


-- | Name of each 'Literal'
type Name = String

-- | There are 3 types of __Implications__
--
-- 'S' : Strict rule .
--
-- 'D' : Defeasible rule .
--
-- 'N' : Rules with no implication: __Ordinary Premises__ or __Axioms__ .
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

-- | name of an instantiation of type `Literal`: it plays two rules:
-- 1. To be used to guarantee the uniqueness of a `Literal`.
-- 2. To be used to defined negation with simple `!`.
name :: Literal a-> Name
name (Rule n _ _ _) = n
name (Atom n _ )    = n
{-
TODO: this implementation limited negation of being strong negation only.
Should be improved.
-}

-- | Body Imp Conc
-- Get body of a rule
body  :: Literal a -> [Literal a]
body (Rule _ b _ _) = b
body (Atom _ _)     = []

-- | Body Imp Conc
-- Get Imp or a rule
imp :: Literal a-> Imp
imp (Rule _ _ i _) = i
imp (Atom _ _ )    = N

-- | Body Imp Conc
-- Get conclusion (head) of a rule
conC :: Literal a -> Literal a
conC (Rule _ _ _ h) = h
conC a@(Atom _ _)   = a

-- | Return the statement of each 'Literal'
--
-- The statement of a 'Rule' is the statement of its head(conclusion)
--
-- The statement of an 'Atom' is the type being contains in this proposition.
--
-- Information in __a__ can be used to define 'ASPIC.Abstraction.NegationFunction'.
-- In the default
statement :: Literal a -> a
statement (Rule _ _ _ h) = statement h
statement (Atom _ a)     = a

branchDef :: forall a.(Eq a) => Argument a-> Language a -> Argument a
branchDef mp [] = []
branchDef mp lang =
    let
        rules = concat mp
        currentLevel = [ r |l <- lang, r <- rules, conC r == l]
        nextLevelPros = concat $ body <$> currentLevel
    in currentLevel : branchDef mp nextLevelPros
