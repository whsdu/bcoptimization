{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ConstraintKinds #-}

module ASPIC.Abstraction where 

import Data.List (group, sort)
import Control.Monad.IO.Class 
import Control.Monad.Reader

import qualified ASPIC.Defeasible as D (Literal, Path, Argument, Board, LogicLanguage(..), Rules(..), SearchRecords, PathRecords, PreferenceMap, SearchRecord, PathRecord)

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
        "length: " ++ show ((length . D.getRules) asRules) 

instance Has (D.LogicLanguage a) (AS a) where obtain  = asLanguage

instance Has (D.Rules a) (AS a) where obtain = asRules

instance Has D.PreferenceMap  (AS a) where obtain = asPreferenceMap

instance Has (PathSelection a) (AS a) where obtain = asPathSelection 

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
type OrderFunction a = D.Path a -> D.Path a -> Bool 

