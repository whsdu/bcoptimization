{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Algorithm.BCauxiliary where 

import Control.Monad (guard)
import qualified Data.HashMap.Strict as Map 
import Data.List (sortBy)
import Data.Function (on)
import Control.Monad.Reader 
import Control.Monad.IO.Class 

import qualified ASPIC.Abstraction as AS
import qualified ASPIC.Defeasible as D 
import qualified ASPIC.Ordering as Ord 
import qualified Run.Env as Env 


{-
lucky :: SearchRecrods 
check each (Path,Defeater) in lucky ( Path, Unwarranted defeater). 
if any new defeater detected. 
move this (Path, New Defeater) to waiting :: PathRecords, 
update seen :: Language, 
get new lucky :: SearchRecords 
-}
checkLuckySet ::
    ( AS.UseASPIC env 
    , AS.Has AS.PathSelection env 
    , AS.Has AS.DefeaterSelection env 
    , MonadIO m 
    , MonadReader env m 
    ) => D.Language -> D.SearchRecords -> m (D.PathRecords, D.Language, D.SearchRecords)
checkLuckySet seen [] = pure ([],seen,[])
checkLuckySet seen (r:rs) = do 
    (re, newSeen) <- checkLucker seen r 
    (newwait, ss , nlucy) <- checkLuckySet newSeen rs 
    case re of 
        Right sr -> pure (newwait,ss, sr : nlucy)
        Left pr -> pure (pr : newwait, ss, nlucy)
    where 

    checkLucker::
        ( AS.UseASPIC env 
        , AS.Has AS.PathSelection env 
        , AS.Has AS.DefeaterSelection env 
        , MonadIO m 
        , MonadReader env m 
        ) => D.Language -> D.SearchRecord -> m (Either D.PathRecord D.SearchRecord, D.Language)
    checkLucker seen sr@(p,_) = do                  
        r <- defeatDetection seen p                      -- previous unwarranted defeater will be discarded. 
        case r of 
            Nothing -> pure (Right sr, seen)
            Just (p,newSeen) -> pure (Left p,newSeen)

    defeatDetection ::
        ( AS.UseASPIC env 
        , AS.Has AS.PathSelection env 
        , AS.Has AS.DefeaterSelection env 
        , MonadIO m 
        , MonadReader env m 
        ) => D.Language -> D.Path -> m (Maybe (D.PathRecord, D.Language))
    defeatDetection seen p = do 
        lang <- D.getLogicLanguage  <$> Env.grab @D.LogicLanguage 
        let 
            valid = [ r | r <- lang, r `notElem` seen ]
            localRules = concat p 
        mapM Ord.conflict valid <*> localRules
        pure undefined 
    
    checkConflict :: 
        ( AS.UseASPIC env 
        , AS.Has AS.PathSelection env 
        , AS.Has AS.DefeaterSelection env 
        , MonadIO m 
        , MonadReader env m 
        ) => D.Literal a-> D.Literal a-> m D.Language 
    checkConflict = undefined 
 
        -- conflicts <- scanAttacker seen p 
        -- defeaters <- concat <$> mapM (checkConflict p) conflicts 
        -- if null defeaters 
            -- then pure (Right sr, seen)
            -- else 
                -- do 
                -- let 
                    -- newSeen = M.rmdups $ seen ++ defeaters      
                -- newPathRecord <- createPathRecord p defeaters   
                -- pure (Left newPathRecord, newSeen) 

-- | TODO: 
-- 1. 'conflict <$> globalRules <*> localRules' is really a computation consuming part!
-- 2. Extend Has so that this can be test automatically using QuickCheck maybe ? 
-- scanAttacker ::
--         ( MonadReader env m
--         , MonadIO m 
--         , Has D.Language env 
--         ) => D.Language -> D.Path -> m [Conflict]
-- scanAttacker seen path = do 
--     lang <- grab @D.Language 
--     let 
--         validRules = [r | r <-lang , r `notElem` seen] 
--         localRules = concat path
    
--     pure $ filter ( /= Peace) $ conflict <$> validRules <*> localRules 

-- checkConflict :: 
--     ( MonadReader env m 
--     , MonadIO m 
--     , Has D.Language env 
--     , UseRuleOnly env 
--     , OrderingContext env 
--     ) => D.Path -> Conflict -> m D.Language 
-- checkConflict _ (Undercut l)= pure [l]
-- checkConflict _ Peace = pure [] 
-- checkConflict p (Rebut l) = do 
--     let 
--         defP = branchDef p [M.neg l]
--     necPaths <- getNecPath l
--     if 
--         null defP 
--         then pure []
--         else  do 
--             rs <- mapM (checkDefeat defP) necPaths 
--             if or rs 
--                 then pure [l] 
--                 else pure []  


-- createPathRecord ::
--         ( MonadReader env m
--         , MonadIO m 
--         , UseRuleOnly env
--         )=> D.Path ->  D.Language -> m PathRecord
-- createPathRecord p defeaters = do 
--     tmpAgu <- initAgu defeaters
--     pure (p,tmpAgu)

-- -- | This is the part which should be further abstracted. 
-- branchDef :: D.Path -> D.Language -> D.Path 
-- branchDef mp [] = []
-- branchDef mp lang = 
--     let 
--         rules = concat mp 
--         currentLevel = [ r |l <- lang, r <- rules, D.conC r == l] 
--         nextLevelPros = concat $ D.body <$> currentLevel
--     in currentLevel : branchDef mp nextLevelPros

-- getNecPath :: 
--     ( MonadReader env m 
--     , MonadIO m 
--     , Has D.Language env 
--     , UseRuleOnly env 
--     ) => D.Literal -> m D.Argument 
-- getNecPath l = do 
--     initA <- initAgu [l]
--     augDefeasible initA

-- checkDefeat :: 
--     ( MonadReader env m 
--     , MonadIO m 
--     , OrderingContext env
--     ) => D.Path -> D.Path -> m Bool 
-- checkDefeat defenderPath attackPath = do 
--     rdPreferenceMap <- D.getRdPrefMap <$>   grab @D.RdPrefMap
--     knPreferenceMap <- D.getKnwlPrefMap <$> grab @D.KnwlPrefMap
--     let prefMap = Map.union rdPreferenceMap knPreferenceMap
--         (flg, dDefs) = lastLinkChecker defenderPath (D.conC <$> head defenderPath)
--     if flg 
--     then
--         do 
--         let 
--             aDefs = [r | r <- concat attackPath, D.imp r == M.D]
--         pure $ ord prefMap aDefs dDefs 
        
--     else 
--         pure flg 

-- selectOneLucker :: SearchRecords -> SearchRecord 
-- selectOneLucker rs = 
--     let 
--         completePath =  [r | r <- rs, reachGround (fst r) ]
--         shortestPath = sortBy (flip compare `on` (length . fst)) completePath 
--     in head shortestPath