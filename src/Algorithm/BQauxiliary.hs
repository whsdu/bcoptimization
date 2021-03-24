{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Algorithm.BQauxiliary where 

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
import qualified Toolkits.Common as TOOL

-- | TODO:
{-
    1 seen should be wrapped in writer monad . 
        This writer monad could be used to store much valuable informatoin. 
        Maybe could just update an term in Reader env . 
    2 defeater construction in getNec should be smarter . 
-}

{-
lucky :: SearchRecrods 
check each (Path,Defeater) in lucky ( Path, Unwarranted defeater). 
if any new defeater detected. 
move this (Path, New Defeater) to waiting :: PathRecords, 
update seen :: Language, 
get new lucky :: SearchRecords 
-}
checkLuckySet ::
    ( AS.Has (D.LogicLanguage a) env
    , AS.Has (D.Rules a) env
    , AS.Has D.PreferenceMap  env
    , AS.Has (AS.OrderFunction a) env
    , AS.Has (AS.CheckNegationFunction a) env
    , AS.Has (AS.NegationFunction a) env
    , MonadIO m 
    , MonadReader env m  
    , Eq a 
    , Show a 
    ) => D.Language a-> D.SearchRecords a-> m (D.PathRecords a, D.Language a, D.SearchRecords a)
checkLuckySet seen [] = pure ([],seen,[])
checkLuckySet seen (r:rs) = do 
    checkResult <- checkLucker seen r 
    liftIO $ print ("check result: " ++ show checkResult)
    case checkResult of 
        Right (sr,newSeen) -> do 
            (newWait, ss, nLucky) <- checkLuckySet newSeen rs 
            pure (newWait, ss , sr : nLucky)
        Left (pr,newSeen) -> do 
            (newWait, ss, nLucky) <- checkLuckySet newSeen rs 
            pure (pr :newWait, ss ,nLucky)
    where 
    -- | Check if Search Record has defeated
    -- | Yes --> Convert SearchRecord to PathRecord , update seen 
    -- | No --> return SearchRecord 
    checkLucker :: forall a env m . 
        ( AS.Has (D.LogicLanguage a) env
        , AS.Has (D.Rules a) env
        , AS.Has D.PreferenceMap  env
        , AS.Has (AS.OrderFunction a) env
        , AS.Has (AS.CheckNegationFunction a) env
        , AS.Has (AS.NegationFunction a) env
        , MonadIO m 
        , MonadReader env m 
        , Eq a
        , Show a
        ) => D.Language a-> D.SearchRecord a-> m (Either (D.PathRecord a, D.Language a) (D.SearchRecord a, D.Language a))
    checkLucker seen sr@(p,_) = do                  
        lang <- D.getLogicLanguage  <$> Env.grab @(D.LogicLanguage a)
        let 
            validP = [ r | r <- lang, r `notElem` seen && D.conC r `notElem` seen ]
            localRules = concat p 
        checkedConflict <- mapM (uncurry Ord.conflict) [(v,l) | v<-validP, l<-localRules]
        let 
            validConflict = filter (/= Ord.Peace) checkedConflict 
        liftIO $ print validConflict
        if null validConflict 
            then pure $ Right (sr, seen)
            else do 
                defeaterPropositions <- concat <$> mapM (checkConflict p) validConflict
                seenUpdate <- concat <$> mapM (updateSeen p) validConflict
                if null defeaterPropositions  
                    then pure $ Right (sr, seen)
                    else do 
                        let 
                            newSeen = TOOL.rmdups $ seen ++ seenUpdate
                        defeaterAgu <- initAgu defeaterPropositions
                        pure $ Left ((p,defeaterAgu), newSeen)

-- | This is a great bug shows why it would be better to use type level logic to restrict behaviour 
-- 原来的 checkConflict 修改后将 attacker 和 defeater proposition 都加入了seen中，变成 defeaterPropositions 来
-- 更新 seen， 但是 没有注意到 defeaterPropositions 同样用来构造 defeaterAgu， 所以如果initAgu 的输入必须满足一定类型的要求
-- terms 或者 types 那么是否可以阻止这个问题？
                        -- let 
                            -- newSeen = TOOL.rmdups $ seen ++ defeaterPropositions
                        -- defeaterAgu <- initAgu defeaterPropositions
                        -- pure $ Left ((p,defeaterAgu), newSeen)
    updateSeen :: forall a env m .
        ( AS.Has (D.LogicLanguage a) env
        , AS.Has (D.Rules a) env
        , AS.Has D.PreferenceMap  env
        , AS.Has (AS.OrderFunction a) env
        , MonadIO m 
        , MonadReader env m 
        , Eq a 
        , Show a 
        ) => D.Path a-> Ord.Conflict a-> m (D.Language a)
    updateSeen _ (Ord.Undercut (a,l))= pure [a]
    updateSeen _ Ord.Peace = pure [] 
    updateSeen p (Ord.Rebut (a,l)) = do 
        let 
            defP = D.branchDef p [D.conC l]
        necPaths <- getNecPath a
        if 
            null defP 
            then pure []
            else  do 
                rs <- mapM (checkDefeat defP) necPaths 
                if or rs 
                    then pure [a,D.conC l] 
                    else pure []   

    checkConflict :: forall a env m .
        ( AS.Has (D.LogicLanguage a) env
        , AS.Has (D.Rules a) env
        , AS.Has D.PreferenceMap  env
        , AS.Has (AS.OrderFunction a) env
        , MonadIO m 
        , MonadReader env m 
        , Eq a 
        , Show a 
        ) => D.Path a-> Ord.Conflict a-> m (D.Language a)
    checkConflict _ (Ord.Undercut (a,l))= pure [a]
    checkConflict _ Ord.Peace = pure [] 
    checkConflict p (Ord.Rebut (a,l)) = do 
        let 
            defP = D.branchDef p [D.conC l]
        necPaths <- getNecPath a
        liftIO $ print necPaths
        liftIO $ print defP
        if 
            null defP 
            then pure []
            else  do 
                rs <- mapM (checkDefeat defP) necPaths 
                liftIO $ print rs
                if or rs 
                    then pure [a] 
                    else pure []   

    getNecPath :: 
        ( AS.Has (D.LogicLanguage a) env
        , AS.Has (D.Rules a) env
        , AS.Has D.PreferenceMap  env
        , MonadIO m 
        , MonadReader env m 
        , Eq a  
        ) => D.Literal a -> m (D.Argument a)
    getNecPath l = do 
        initA <- initAgu [l]
        augDefeasible initA

    checkDefeat :: forall a env m.
        ( AS.Has D.PreferenceMap  env
        , AS.Has (AS.OrderFunction a) env
        , MonadIO m 
        , MonadReader env m 
        ) => D.Path a-> D.Path a-> m Bool 
    checkDefeat defenderPath attackPath = do 
        prefMap <- Env.grab @D.PreferenceMap 
        ordering <- Env.grab @(AS.OrderFunction a)
        pure $ ordering prefMap attackPath defenderPath 

{-Construction Auxiliary-}
initAgu :: 
        ( AS.Has (D.Rules a) env
        , MonadIO m 
        , MonadReader env m 
        , Eq a  
        )=> D.Language a -> m (D.Argument a)
initAgu ls = do
    subLevel <- mapM concludeBy ls 
    pure [[p] | p <- foldr createParallel [[]] subLevel]

concludeBy ::forall a env m.
        ( AS.Has (D.Rules a) env
        , MonadIO m 
        , MonadReader env m  
        , Eq a 
        ) => D.Literal a -> m (D.Language a)
concludeBy l = do 
    rules <- D.getRules <$> Env.grab @(D.Rules a)
    pure [r | r<- rules, D.conC r == l ]

aguFixpoint :: 
    ( AS.Has (D.Rules a) env
    , MonadIO m 
    , MonadReader env m 
    , Eq a 
    ) => D.Argument a -> m (D.Argument a)
aguFixpoint argument = do 
    extendedAgu <- agu argument 
    if extendedAgu == argument 
        then pure argument 
        else aguFixpoint extendedAgu 

augDefeasible :: 
    ( AS.Has (D.Rules a) env
    , MonadIO m 
    , MonadReader env m 
    , Eq a 
    ) => D.Argument a-> m (D.Argument a)
augDefeasible = aguFixpoint 

agu :: 
    ( AS.Has (D.Rules a) env
    , MonadIO m 
    , MonadReader env m 
    , Eq a 
    ) => D.Argument a-> m (D.Argument a)
agu argument = do 
        arguments <- mapM pathExtend argument 
        pure $ concat arguments 

pathExtend :: 
    ( AS.Has (D.Rules a) env
    , MonadIO m 
    , MonadReader env m
    , Eq a 
    ) => D.Path a -> m (D.Argument a)
pathExtend path = do 
    let 
        rules = last path 
        bodies = concat $ D.body <$> rules 
    if 
        null bodies 
        then pure [path]
    else 
        do 
        supportRules <- mapM concludeBy bodies 
        let 
            parallelPathSection = foldr createParallel [[]] supportRules
            newArgument = do 
                    pathSection <- parallelPathSection
                    pure $ path ++ [pathSection] 
        pure newArgument

createParallel :: [a] -> [[a]] -> [[a]] 
createParallel paths ls = do 
        pa <- paths
        a <- ls 
        pure $  pa:a  

checkLuckyComplete :: D.SearchRecords a-> D.SearchRecords a
checkLuckyComplete rs = [r | r <- rs, reachGround (fst r) ]

reachGround :: D.Path a-> Bool 
reachGround path = 
    let 
        i = last path 
    in case concat (D.body <$> i) of 
        [] -> True 
        _ -> False 

pickOneFromManyLucker :: D.SearchRecords a-> D.SearchRecord a
pickOneFromManyLucker rs = 
    let 
        completePath =  [r | r <- rs, reachGround (fst r) ]
        shortestPath = sortBy (flip compare `on` (length . fst)) completePath 
    in head shortestPath