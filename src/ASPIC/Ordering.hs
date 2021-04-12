{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module ASPIC.Ordering where 

import ASPIC.Defeasible (Path, Imp(D, S, N), Literal, imp, conC ) 
import ASPIC.Abstraction 
import Run.Env 

import Data.Maybe (fromMaybe)
import Control.Monad.IO.Class 
import Control.Monad.Reader 


data Conflict a = Rebut (Literal a, Literal a)| Undercut (Literal a, Literal a)| Peace deriving (Eq)

instance Show a => Show (Conflict a) where 
    show (Rebut (a, d)) = show a ++ " rebuts " ++ show d 
    show (Undercut (a, d)) = show a ++ " undercuts " ++ show d 
    show Peace = "Peace!"


conflict :: 
    ( Eq a
    , MonadReader env m 
    , MonadIO m 
    , Has (NegationFunction a) env
    , Has (CheckNegationFunction a) env 
    ) => Literal a -> Literal a -> m (Conflict a)
conflict proposition rule = 
    case imp rule of 
        S -> pure Peace 
        N -> pure Peace 
        D -> do 
            r1 <- rebuts proposition rule 
            r2 <- undercuts proposition rule 
            pure $ fromMaybe (fromMaybe Peace r1) r2

rebuts :: forall a env m . 
    ( Eq a
    , MonadReader env m 
    , MonadIO m 
    , Has (NegationFunction a) env
    , Has (CheckNegationFunction a) env 
    ) => Literal a -> Literal a -> m (Maybe (Conflict a))
rebuts proposition rule = do 
    neg <- grab @(NegationFunction a)
    isNeg <- grab @(CheckNegationFunction a)
    if isNeg (neg proposition) (conC rule)
        then pure $ Just $ Rebut (proposition , rule)
        else pure Nothing  

undercuts:: forall a env m. 
    ( Eq a
    , MonadReader env m 
    , MonadIO m 
    , Has (NegationFunction a) env
    , Has (CheckNegationFunction a) env 
    ) => Literal a -> Literal a -> m (Maybe (Conflict a))
undercuts ruleAttacker rule = do 
    neg <- grab @(NegationFunction a)
    isNeg <- grab @(CheckNegationFunction a)
    if isNeg ((neg . conC) ruleAttacker) rule 
        then pure $ Just $ Undercut (conC ruleAttacker, rule) 
        else pure Nothing 

isOrdReady :: forall a . Path a -> Bool 
isOrdReady p = undefined 

