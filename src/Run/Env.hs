{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Run.Env where 

import Control.Monad.Reader
    ( MonadIO, asks, MonadReader, ReaderT(..) )

import qualified ASPIC.Abstraction as AS (Negation(..), Has(..),SelectionFunction,AS)


grab :: forall field env m . (MonadReader env m , AS.Has field env) => m field 
grab = asks $ AS.obtain @field 

newtype App a = App 
    { unApp :: ReaderT AS.AS IO a 
    } deriving newtype (Functor, Applicative, Monad, MonadIO , MonadReader AS.AS) 

runApp :: AS.AS -> App a -> IO a 
runApp env app = (runReaderT $ unApp app) env 