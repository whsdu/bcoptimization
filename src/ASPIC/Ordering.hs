{- | The name of this module is somehow misleading. 
In this context, ordering among arguments exists only because they conflict with each other.
Therefore, in this module all data type and functions are used for describing conflicts between arguments. 
-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeApplications          #-}

module ASPIC.Ordering 
    ( Conflict(..)
    , conflict 
    , rebuts 
    , undercuts
    , isOrdReady
    )where

import           ASPIC.Abstraction      (CheckNegationFunction, Has,
                                         NegationFunction)
import           ASPIC.Defeasible       (Imp (D, N, S), Literal, Path, conC,
                                         imp)
import           Run.Env                (grab)

import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Reader   (MonadIO, MonadReader)
import           Data.Maybe             (fromMaybe)

-- | Conflict of 
data Conflict a = Rebut (Literal a, Literal a)| Undercut (Literal a, Literal a)| Peace deriving (Eq)

instance Show a => Show (Conflict a) where
    show (Rebut (a, d))    = show a ++ " rebuts " ++ show d
    show (Undercut (a, d)) = show a ++ " undercuts " ++ show d
    show Peace             = "Peace!"


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

-- | 
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
{-
TODO: all these functions should not be included in the monad
So that it won't need to relies on Env, thus could be safe and easy to be tested. 
-}

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

