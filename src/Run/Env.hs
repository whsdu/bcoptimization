{-|
Functions and Type (App) being used for ReaderT pattern
-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuantifiedConstraints      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Run.Env
    ( grab
    , runApp
    , App(..)
    )where

import           Control.Monad.Reader (MonadIO, MonadReader, ReaderT (..), asks)

import qualified ASPIC.Abstraction    as AS (AS, Has (..))

-- | Combinator that connect MonadReader and Has
grab :: forall field env m . (MonadReader env m , AS.Has field env) => m field
grab = asks $ AS.obtain @field

newtype App a b = App
    { unApp :: ReaderT (AS.AS a) IO b
    } deriving newtype (Functor, Applicative, Monad, MonadIO , MonadReader (AS.AS a))

runApp :: AS.AS a -> App a b -> IO b
runApp env app = (runReaderT $ unApp app) env
