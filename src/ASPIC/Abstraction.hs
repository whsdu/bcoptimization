{-|
This module contains information being usd for

- Argumentation System (AS)
- Function Types that parameterize the behaviour of Backward Chaining Algorithm
- Type Class that support ReaderT(Has) pattern.

-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE QuantifiedConstraints     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE Safe                      #-}
{-# LANGUAGE TypeSynonymInstances      #-}
-- {-# LANGUAGE ConstraintKinds #-}

module ASPIC.Abstraction
    (
    -- * Argumentation System
     AS(..)
    -- * Parametrize critical function that controls the behaviour of BC algorithm
    , PathSelection
    , DefeaterSelection
    , NegationFunction
    , CheckNegationFunction
    , OrderFunction
    -- * Has pattern
    , Has(..)
    )where

import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.HashMap.Strict    (toList)
import           Data.List              (group, sort)

import qualified ASPIC.Defeasible       as D (Argument, Board, Literal,
                                              LogicLanguage (..), Path,
                                              PathRecord, PathRecords,
                                              PreferenceMap, Rules (..),
                                              SearchRecord, SearchRecords)

-- | 'AS' is short for Argumentation System.
--
-- It is a polymorphic data type, means proposition could be of any customized type.

data AS a =  AS
    { asLanguage              :: D.LogicLanguage a
    , asRules                 ::  D.Rules a
    , asPreferenceMap         :: D.PreferenceMap
    , asPathSelection         ::  PathSelection a
    , asDefeaterSelection     ::  DefeaterSelection a
    , asNegationFunction      ::   NegationFunction a
    , asCheckNegationFunction ::   CheckNegationFunction a
    , asOrderFunction         :: OrderFunction a
    }

-- | Has pattern

class Has field env  where
    obtain :: env -> field

instance (Show a) => Show (AS a) where
    show AS{..} =
        "Logic Language : " ++ show asLanguage ++ "\n" ++
        "length: " ++ show ((length . D.getLogicLanguage) asLanguage) ++ "\n" ++
        "Rules are: " ++ show asRules ++ "\n" ++
        "length: " ++ show ((length . D.getRules) asRules)  ++ "\n" ++
        "Preference Map is: " ++ show asPreferenceMap ++ "\n" ++
        "length: " ++ show ((length . toList) asPreferenceMap)

instance Has (D.LogicLanguage a) (AS a) where obtain  = asLanguage
instance Has (D.LogicLanguage a) (D.LogicLanguage a) where obtain  = id 

instance Has (D.Rules a) (AS a) where obtain = asRules
instance Has (D.Rules a) (D.Rules a) where obtain = id 

instance Has D.PreferenceMap  (AS a) where obtain = asPreferenceMap
instance Has D.PreferenceMap  D.PreferenceMap where obtain = id  

instance Has (PathSelection a) (AS a) where obtain = asPathSelection
instance Has (PathSelection a) (PathSelection a)  where obtain = id  

instance Has (DefeaterSelection a) (AS a) where obtain = asDefeaterSelection
instance Has (DefeaterSelection a) (DefeaterSelection a) where obtain = id  

instance Has (NegationFunction a) (AS a) where obtain = asNegationFunction
instance Has (NegationFunction a) (NegationFunction a) where obtain = id

instance Has (CheckNegationFunction a) (AS a) where obtain = asCheckNegationFunction
instance Has (CheckNegationFunction a) (CheckNegationFunction a) where obtain = id

instance Has (OrderFunction a) (AS a) where obtain = asOrderFunction
instance Has (OrderFunction a) (OrderFunction a) where obtain = id


-- | Type of functions that select one search records from a set of search-records.
--
-- The incomplete-argument of the selected search record will be extended.

type PathSelection a =  D.SearchRecords a-> (D.SearchRecord a, D.SearchRecords a)

-- | Type of functions that select one path record form a set of path-records.
--
-- The undecided defeater of the selected path record will be extended.

type DefeaterSelection a =  D.PathRecords a -> (D.PathRecord a, D.PathRecords a)

{-
TODO: Could these two selections above be described using servant like DSL method ?
-}



-- | Define how to convert a literal to its negation.
type NegationFunction a =  D.Literal a -> D.Literal a

-- | Defined how to check if two literals are negate each other.
type CheckNegationFunction a =  D.Literal a -> D.Literal a -> Bool

-- | Defined the priority orderings relation between two incomplete-arguments.
type OrderFunction a = D.PreferenceMap  -> D.Path a -> D.Path a -> Bool
{-
TODO:
The ability to define Negation when implementing this library might relies on
the use of type class, just like Servant .
...妈蛋，还有n多要搞哦
1.
-}

