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

import qualified ASPIC.AS  as AS (LogicLanguage(..),Rules(..))
import qualified ASPIC.Abstraction as Abs (Negation(..))

data AS = AS 
    { asLanguage :: AS.LogicLanguage 
    , asRules :: AS.Rules 
    , conflict ::  forall a. (Abs.Negation a) => a -> a -> Bool 
    } 

instance Show AS where 
    show as = 
        "Language: " ++ show (asLanguage as) ++ "\n" ++
        "Rules: " ++ show (asRules as)  


class Has field env  where 
    obtain :: env -> field 

instance Has AS.LogicLanguage  AS where obtain = asLanguage 
instance Has AS.Rules AS where obtain = asRules 

type UseASPIC env = (Has AS.LogicLanguage env, Has AS.Rules env)

grab :: forall field env m . (MonadReader env m , Has field env) => m field 
grab = asks $ obtain @field 

newtype App a = App 
    { unApp :: ReaderT AS IO a 
    } deriving newtype (Functor, Applicative, Monad, MonadIO , MonadReader AS) 

runApp :: AS -> App a -> IO a 
runApp env app = (runReaderT $ unApp app) env 