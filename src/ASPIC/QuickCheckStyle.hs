{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module ASPIC.QuickCheckStyle where 

import Control.Monad.Reader ( MonadReader , MonadIO) 

import qualified ASPIC.Abstraction as As
import qualified ASPIC.Defeasible as D 

type Binary' = forall a env m. 
               ( As.Has (D.Rules a) env
               , MonadReader env m
               , MonadIO m)
               => D.Language a -> m (D.Language a)

expand :: Binary'
expand = pure undefined  

{------}
data PartialArgument 

newtype RelationalTree = RelationalTree PartialArgument

newtype AT = AT PartialArgument 


{- 
B ( a, b ), semantics is $\forall a:: A, there exists b :: A$
thus, should exists f, s.t. B ( a, f b) holds
solution 2: explicitly provides this `f` ( domain is AT).
solution 1: on the other hand assume that AT may be not big enough and thus 
-}


class Context a context where 
    inContext :: a -> context -> Bool 

instance Context RelationalTree AT where 
    inContext _ _ = True 


-- | solution 1: not sure about if there exists on in $\AT$ 
binary :: RelationalTree -> (RelationalTree -> Bool)
binary a _ = case binary' a of 
            Nothing -> False 
            Just _ -> True 
-- binary = undefined 

-- | solution 2: provide a witness that satisfy the Binary relation.
binary' :: RelationalTree -> Maybe RelationalTree
binary' = undefined  

-- B(a,b) implies forall a , exists b , s.t B(a,b) holds, there is f a = b , ( a, f(a) )
-- T(a,b,c) implies forall a, exists b ,c , s.t. T(a,b,c) holds, there f a= (b,c) 