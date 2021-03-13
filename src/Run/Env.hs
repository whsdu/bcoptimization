{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Run.Env where 

import Control.Monad.Reader
    ( MonadIO, asks, MonadReader, ReaderT(..) )

import qualified ASPIC.Abstraction as AS ( Has(..),AS)


grab :: forall field env m . (MonadReader env m , AS.Has field env) => m field 
grab = asks $ AS.obtain @field 

newtype App a b = App 
    { unApp :: ReaderT (AS.AS a) IO b
    } deriving newtype (Functor, Applicative, Monad, MonadIO , MonadReader (AS.AS a)) 

runApp :: AS.AS a -> App a b -> IO b
runApp env app = (runReaderT $ unApp app) env 