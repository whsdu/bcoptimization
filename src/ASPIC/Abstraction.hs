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

import qualified ASPIC.Defeasible as D(Literal, Board, LogicLanguage(..), Rules(..), SearchRecords, PathRecords)

data AS a =  AS 
    { asLanguage :: D.LogicLanguage a
    , asRules ::  D.Rules a
    , asPathSelection ::  PathSelection a
    , asDefeaterSelection ::  DefeaterSelection a
    , asNegationFunction ::   NegationFunction a
    , asCheckNegationFunction ::   CheckNegationFunction a
    } 

class Has field env  where 
    obtain :: env -> field 

instance (Show a) => Show (AS a) where 
    show AS{..} = show asLanguage ++ show asRules

instance Has (D.LogicLanguage a) (AS a) where obtain  = asLanguage

instance Has (D.Rules a) (AS a) where obtain = asRules

instance Has (PathSelection a) (AS a) where obtain = asPathSelection 

instance Has (NegationFunction a) (AS a) where obtain = asNegationFunction

instance Has (CheckNegationFunction a) (AS a) where obtain = asCheckNegationFunction

type PathSelection a = D.Rules a-> D.SearchRecords a-> Bool 
type DefeaterSelection a = D.Rules a -> D.PathRecords a -> Bool 

-- type ASPIC a = (Has (D.LogicLanguage  a) (AS a), Has (D.Rules a) (AS a))


-- | The ability to define Negation when implementing this library might relies on 
-- the use of type class, just like Servant . 
-- ...妈蛋，还有n多要搞哦
type NegationFunction a =  D.Literal a -> D.Literal a 
type CheckNegationFunction a =  D.Literal a -> D.Literal a -> Bool 

