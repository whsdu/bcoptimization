{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module ASPIC.Abstraction where 

import Data.List (group, sort)
import Control.Monad.IO.Class 
import Control.Monad.Reader

import qualified ASPIC.Defeasible as D(Board, LogicLanguage(..), Rules(..))

data AS = AS 
    { asLanguage :: D.LogicLanguage 
    , asRules :: D.Rules 
    , asSelection :: SelectionFunction
    } 

instance Show AS where 
    show as = 
        "Language: " ++ show (asLanguage as) ++ "\n" ++
        "Rules: " ++ show (asRules as)  

class Has field env  where 
    obtain :: env -> field 

instance Has D.LogicLanguage  AS where obtain = asLanguage 
instance Has D.Rules AS where obtain = asRules 

type UseASPIC env = (Has D.LogicLanguage env, Has D.Rules env)

class (Eq a) => Negation a where 
    neg :: a -> a 
    negation :: a -> a -> Bool 
    negation a1 a2 = neg a1 == a2

type SelectionFunction = forall m env. (MonadIO m, MonadReader env m, UseASPIC env ) => D.Board -> m Bool 

