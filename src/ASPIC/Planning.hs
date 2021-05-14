{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes#-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module ASPIC.Planning where 

import Data.Kind(Type) 
import qualified Data.HashMap.Strict as Map 
import qualified ASPIC.Defeasible as D 
import qualified Run.Env                as Env
import qualified ASPIC.Abstraction      as AS

type BinaryRules = forall a env m. (AS.Has (D.Rules a) env) => D.Language a -> m (D.ArgumentGroup a)

type BinaryRelation = forall a env m. (AS.Has (D.Rules a) env) => D.Argument a-> m (D.ArgumentGroup a)

type TernaryRelation = forall a env m. (AS.Has (D.Rules a) env) => D.Argument a-> m [(D.Argument a, D.Argument a)]

data RelationalTree a = Discard () | Single (D.Argument a ) | Tree (D.Argument a) (RelationalTree a)


data Goal = Int | Bool 

type Heuristic = forall a . RelationalTree a -> Goal 

newtype Atom = Atom {atomName :: String}
data Rule = Rule {ruleName :: String, body :: [Atom] , head :: Atom} 


{-
0. define partial argument ( put constraint on this ? )
1. define n-ary relation. ( constructive 1 -> n )
2. define relational tree 
3. define goal :: relational tree -> Goal 
    heuristic function that can be integrated with other heuristic planning 
-}

-- to 
-- design according to the tutorial 
type L = [Atom]
type R = [Rule]

data PartialArgument a where 
    APartial :: PartialArgument Atom 
    -- (:>) ::  

n :: Map.HashMap Atom Rule 
n = Map.empty


-- initAgu ::
--         ( AS.Has (D.Rules a) env
--         , MonadIO m
--         , MonadReader env m
--         , Eq a
--         )=> D.Language a -> m (D.ArgumentGroup a)
-- initAgu ls = do
--     subLevel <- mapM concludeBy ls
--     pure [[p] | p <- foldr createParallel [[]] subLevel
-- 
-- agu ::
--     ( AS.Has (D.Rules a) env
--     , MonadIO m
--     , MonadReader env m
--     , Eq a
--     ) => D.ArgumentGroup a-> m (D.ArgumentGroup a)
-- agu argument = do
--         arguments <- mapM pathExtend argument
--         pure $ concat arguments
-- 
-- pathExtend ::
--     ( AS.Has (D.Rules a) env
--     , MonadIO m
--     , MonadReader env m
--     , Eq a
--     ) => D.Argument a -> m (D.ArgumentGroup a)
-- pathExtend path = do
--     let
--         rules = last path
--         bodies = concat $ D.body <$> rules
--     if
--         null bodies
--         then pure [path]
--     else
--         do
--         supportRules <- mapM concludeBy bodies
--         let
--             parallelPathSection = foldr createParallel [[]] supportRules
--             newArgument = do
--                     pathSection <- parallelPathSection
--                     pure $ path ++ [pathSection]
--         pure newArgument