{-|
This module contains information being usd for 

- Argumentation System (AS)
- Function Types that parameterize the behaviour of Backward Chaining Algorithm 
- Type Class that support ReaderT(Has) pattern. 

-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
-- {-# LANGUAGE ConstraintKinds #-}

module ASPIC.Abstraction 
    (
    -- * Argumentation System 
     AS(..)
    -- * Parametrization Critical Behaviour Function of BC algorithm 
    , PathSelection
    , DefeaterSelection 
    , NegationFunction 
    , CheckNegationFunction 
    , OrderFunction 
    -- * Environment using Has pattern
    , Has(..)
    )where 

import Data.HashMap.Strict (toList)
import Data.List (group, sort)
import Control.Monad.IO.Class 
import Control.Monad.Reader

import qualified ASPIC.Defeasible as D (Literal, Path, Argument, Board, LogicLanguage(..), Rules(..), SearchRecords, PathRecords, PreferenceMap, SearchRecord, PathRecord)

-- | 'AS' is short for Argumentation System. 
data AS a =  AS 
    { asLanguage :: D.LogicLanguage a
    , asRules ::  D.Rules a
    , asPreferenceMap :: D.PreferenceMap
    , asPathSelection ::  PathSelection a
    , asDefeaterSelection ::  DefeaterSelection a
    , asNegationFunction ::   NegationFunction a
    , asCheckNegationFunction ::   CheckNegationFunction a
    , asOrderFunction :: OrderFunction a
    } 

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

instance Has (D.Rules a) (AS a) where obtain = asRules

instance Has D.PreferenceMap  (AS a) where obtain = asPreferenceMap

instance Has (PathSelection a) (AS a) where obtain = asPathSelection 

instance Has (DefeaterSelection a) (AS a) where obtain = asDefeaterSelection

instance Has (NegationFunction a) (AS a) where obtain = asNegationFunction

instance Has (CheckNegationFunction a) (AS a) where obtain = asCheckNegationFunction

instance Has (OrderFunction a) (AS a) where obtain = asOrderFunction


-- | TODO: 
-- Is it possible that this type relies on the Env above ?
type PathSelection a =  D.SearchRecords a-> (D.SearchRecord a, D.SearchRecords a)
type DefeaterSelection a =  D.PathRecords a -> (D.PathRecord a, D.PathRecords a)

-- type ASPIC a = (Has (D.LogicLanguage  a) (AS a), Has (D.Rules a) (AS a))


-- | The ability to define Negation when implementing this library might relies on 
-- the use of type class, just like Servant . 
-- ...妈蛋，还有n多要搞哦
type NegationFunction a =  D.Literal a -> D.Literal a 
type CheckNegationFunction a =  D.Literal a -> D.Literal a -> Bool 
type OrderFunction a = D.PreferenceMap  -> D.Path a -> D.Path a -> Bool 

