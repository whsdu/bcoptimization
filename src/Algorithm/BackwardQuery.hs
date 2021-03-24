{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Algorithm.BackwardQuery where 



import Control.Monad (guard)
import qualified Data.HashMap.Strict as Map 
import Data.List (sortBy)
import Data.Function (on)
import Control.Monad.Reader 
import Control.Monad.IO.Class 

import qualified ASPIC.Abstraction as AS
import qualified ASPIC.Defeasible as D 
import qualified Run.Env as Env 

import Algorithm.BQauxiliary 
                            ( checkLuckySet, checkLuckyComplete, pickOneFromManyLucker
                            , pathExtend)

query ::
    ( AS.Has (D.LogicLanguage a) env
    , AS.Has (D.Rules a) env
    , AS.Has D.PreferenceMap  env
    , AS.Has (AS.OrderFunction a) env
    , AS.Has (AS.CheckNegationFunction a) env
    , AS.Has (AS.NegationFunction a) env
    , AS.Has (AS.PathSelection a) env
    , AS.Has (AS.DefeaterSelection  a) env
    , MonadIO m 
    , MonadReader env m  
    , Eq a
    , Show a 
    ) => D.Argument a->  m (D.Defeater a, D.Language a)
query incArgu = queryArgument incArgu []

-- | rewrite query 
queryArgument ::
    ( AS.Has (D.LogicLanguage a) env
    , AS.Has (D.Rules a) env
    , AS.Has D.PreferenceMap  env
    , AS.Has (AS.OrderFunction a) env
    , AS.Has (AS.CheckNegationFunction a) env
    , AS.Has (AS.NegationFunction a) env
    , AS.Has (AS.PathSelection a) env
    , AS.Has (AS.DefeaterSelection  a) env
    , MonadIO m 
    , MonadReader env m  
    , Eq a
    , Show a  
    ) => D.Argument a-> D.Language a->  m (D.Defeater a, D.Language a)
queryArgument incArgu accSeen = do 
    let 
        tmpBoard = initialBoard incArgu 
        initBoard = tmpBoard{D.seen=accSeen}
    defeatChain initBoard 

-- | rewrite of chain construction 
defeatChain :: 
    ( AS.Has (D.LogicLanguage a) env
    , AS.Has (D.Rules a) env
    , AS.Has D.PreferenceMap  env
    , AS.Has (AS.OrderFunction a) env
    , AS.Has (AS.CheckNegationFunction a) env
    , AS.Has (AS.NegationFunction a) env
    , AS.Has (AS.PathSelection a) env
    , AS.Has (AS.DefeaterSelection  a) env
    , MonadIO m 
    , MonadReader env m  
    , Eq a
    , Show a 
    )  => D.Board a -> m (D.Defeater a, D.Language a)
defeatChain step1Board = do 
    liftIO $ putStrLn ""
    liftIO $ putStrLn "Enter DCHAIN!"
    liftIO $ putStrLn ("Inital Board: " ++ show step1Board )
    step1 <- defeatDetection step1Board 
    liftIO $ print step1
    case step1 of 
        Right p -> pure  (warranted p, D.seen step1Board)
        Left step2Board -> do 
            step3 <- pathSelection step2Board 
            case step3 of 
                Just (base, step3Board) ->  do 
                    luckyExtend <- aguConstruction base 
                    let 
                        step3Lucky = D.lucky step3Board
                        step4Board = step3Board{D.lucky=step3Lucky++luckyExtend}
                    defeatChain step4Board
                Nothing -> do 
                    step6 <- defeaterChain step2Board 
                    case step6 of 
                        Right unw -> pure (unw, D.seen step2Board)
                        Left step6Board -> defeatChain step6Board


{- 1
-- Intialize a given set of rules (this set of rules concludes to a set of proposition , old equifinal path section) 
as an incomplete-argument (incArgument). 
        - Initialize these in-complete paths SearchRecord == 'lucky' (each SearchRecord has a Defeater as 'Node[]')
        - Initialize empty PathRecords      [] == 'waiting' 
        - Initialize empty SearchRecords    [] == 'futile'  
Notes: 
    1. incArgu is of type Argument = [Paths,...,Path] 
    2. Each Path in [Paths,...,Path] is a head of an incomplete-path.
    3. Path in [Paths,...,Path] are all [Rules], and Rules has same set of heads. 
-}
-- go to 2 
initialBoard :: (Show a) => D.Argument a -> D.Board a
initialBoard incArgu = 
    let 
        luckySet = (,D.NoDefeater) <$> incArgu 
    in D.Board luckySet [] [] []

{- 2
- Given Board from step 1
    - Check 'lucky': SearchRecords in lucky as of the type ::  [(Path a,Defeater a)]

        - include all newly defeated target to seen
        - Attaches the inc-defeater to related Path , move this PathRecord to 'waiting'
        - If path has old related defeater(much be unwarranted defeater), then remove this old defeater and attach this new in-defeater. 
            - a SearchRecord turns to be a PathRecord
    - Check if any SearchRecord in 'lucky' contains complete Path. 
        - Yes: go to 5 
            - Exit : Pass the complete Path Record to 5 and get return result 
        - No : go to 3 
            - Continue construction
Notes:
    1. when detecting one search record in lucky has new in-defeater, remove old fail-defeater, then put to tmpWaiting
        inc-Argument defeat path
    2. select one lucker from possibly many in lucky
-}
defeatDetection ::
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
    ) => D.Board a -> m (Either (D.Board a) (D.SearchRecord a))
defeatDetection board@D.Board{..} = do 
        (newWaiting, newSeen, newLucky) <- checkLuckySet seen lucky
        let 
            completeLuckers = checkLuckyComplete newLucky 
        if (not . null) completeLuckers 
            then pure $ Right $ pickOneFromManyLucker completeLuckers 
            else pure $ Left $ board{D.lucky=newLucky, D.waiting=waiting ++newWaiting, D.seen =newSeen}

{- 3
- Given Board from step 2
    - If 'lucky' contains more than one SearchRecord, select One (Path selection strategy)
        - Set this one as 'Base' :: SearchRecord 
        - Move this SearchRecord out of lucky 
        - go to 4 == Just SearchRecord
    - If 'lucky' is empty 
        - go to 6 == Nothing 
-}
pathSelection ::forall a env m. 
    ( AS.Has (D.LogicLanguage a) env
    , AS.Has (D.Rules a) env
    , AS.Has (AS.PathSelection a) env  
    , MonadIO m 
    , MonadReader env m 
    )=> D.Board a-> m (Maybe (D.SearchRecord a, D.Board a))
pathSelection board@D.Board{..} = case lucky of 
    [] -> pure Nothing 
    _ ->  do 
        selectionOne <- Env.grab @(AS.PathSelection a)
        let 
            (base, newlucky) = selectionOne lucky 
            newBoard = board{D.lucky=newlucky}
        pure $ Just (base,newBoard)

-- selectionOne ::
--     ( AS.Has (D.LogicLanguage a) env
--     , AS.Has (D.Rules a) env
--     , AS.Has (AS.PathSelection a) env  
--     , AS.Has (AS.DefeaterSelection a) env  
--     , MonadIO m 
--     , MonadReader env m 
--     )=> D.SearchRecords a-> m (D.SearchRecord a, D.SearchRecords a)
-- selectionOne srs = undefined  
-- -- selectionOne srs = do 
--     -- let 
--         -- sortByLength = sortBy (flip compare `on` (length . fst)) srs 
--         -- sortByDefeasible = sortBy (flip compare `on` (length . getDef . fst)) srs 
--     -- pure (head sortByDefeasible, tail sortByDefeasible)  
--     -- where 
--         -- getDef :: D.Path -> D.Language 
--         -- getDef p = 
--             -- let 
--                 -- rules = concat p 
--             -- in [r | r<-rules, D.imp r == M.D]

{- 4
- Given SearchRecord selected from step 3, continue construction. 
    - More path may be discovered, thus append existing Defeater to all new paths. 
    - These new paths are New SearchRecords.
- Get a new 'lucky' = New SearchRecords + old lucky (SearchRecords). 

- get New Board with new 'lucky' from Board in step 3 
- go to step 2 (input new Board)
-}
aguConstruction ::
    ( AS.Has (D.LogicLanguage a) env
    , AS.Has (D.Rules a) env
    , MonadIO m 
    , MonadReader env m 
    , Eq a 
    ) => D.SearchRecord a -> m (D.SearchRecords a)
aguConstruction (p,defeater) = do 
    tmpArg <- pathExtend p 
    pure $ (,defeater) <$> tmpArg 

{- 5 : Defeater Return 
- Given a complete path record :: PathRecord. 
    - if SearchRecord related argument is Node [] . 
        - return SW Path 
    - else 
        - return Node [(Path, Defeater)]
        - convert SearchRecord :: (Path, Defeater) to Node [(Path, Defeater)]
        - In this case, existing Defeater must be a non-warranted argument (Saved from waiting). 
-}
warranted :: (Show a) => D.SearchRecord a -> D.Defeater a
warranted (p, D.NoDefeater) = D.SW [p]
warranted record = D.Warranted record 

{- 6 
Known 'lucky' is empty  from step 3
- Select a PathRecord from 'waiting' according to Step 10
- The purpose is to check whether the defeater is Warranted. 
    - Construct the defeater and check defeater return , if the defeater is Warranted . 
        - go to 7 
    - If 'waiting' is empty because of step 7 and step 9
        - go to  step 8 
    - Construct the defeater and check defeater return, if the Defeaters is UnWarranted. 
        - go to 9 
-}
defeaterChain :: forall a env m. 
    ( AS.Has (D.LogicLanguage a) env
    , AS.Has (D.Rules a) env
    , AS.Has D.PreferenceMap  env
    , AS.Has (AS.OrderFunction a) env
    , AS.Has (AS.CheckNegationFunction a) env
    , AS.Has (AS.NegationFunction a) env
    , AS.Has (AS.PathSelection a) env
    , AS.Has (AS.DefeaterSelection  a) env
    , MonadIO m 
    , MonadReader env m  
    , Eq a
    , Show a  
    ) => D.Board a-> m (Either (D.Board a) (D.Defeater a))
defeaterChain board@D.Board{..} = 
    case waiting of 
        [] -> pure $ Right $ unwarranted futile 
        _ -> do 
            selectionTwo <- Env.grab @(AS.DefeaterSelection a)
            let 
                ((p,incArgument), tmpWaiting) = selectionTwo waiting 
            (result,defeaterSeen) <- queryArgument incArgument seen 
            liftIO $ print ("Reulst: " ++ show result)
            liftIO $ print ("TmpWaiting: " ++ show tmpWaiting)
            case result of 
                (D.Warranted _)->  do
                    let 
                        newBoard = waitingToFutile (p,result) tmpWaiting defeaterSeen board
                    defeaterChain newBoard
                (D.SW _) -> do 
                    let 
                        newBoard = waitingToFutile (p,result) tmpWaiting defeaterSeen board
                    defeaterChain newBoard
                (D.Unwarranted _)-> do 
                    let 
                        newBoard = survived (p,result) tmpWaiting defeaterSeen board
                    pure $ Left newBoard 
-- 
-- selectionTwo ::
--     ( AS.Has (D.LogicLanguage a) env
--     , AS.Has (D.Rules a) env
--     , AS.Has (AS.PathSelection a) env  
--     , AS.Has (AS.DefeaterSelection a) env  
--     , MonadIO m 
--     , MonadReader env m 
--     )=> D.PathRecords a-> m (D.PathRecord a, D.PathRecords a)
-- selectionTwo prs = do 
--     let 
--         sortByLength = sortBy (flip compare `on` (length . fst)) prs 
--         sortByDefeasible = sortBy (flip compare `on` (length . getDef . fst)) prs 
--     pure (head sortByDefeasible, tail sortByDefeasible)  
--     where 
--         getDef :: D.Path a-> D.Language a
--         getDef p = 
--             let 
--                 rules = concat p 
--             in [r | r<-rules, D.imp r == D.D]
            

-- {- 7
-- In step 6, given the defeater is Warranted. We have a new SearchRecord  
--     - Remove old PathRecord from 'waiting'
--     - Move new SearchRecord  to 'futile'
--         - go back to 6 
-- -}
-- -- TODO: futile should be [(Path, Defeater)]
waitingToFutile :: D.SearchRecord a-> D.PathRecords a->D.Language a-> D.Board a-> D.Board a
waitingToFutile newFutile newWaiting defeaterSeen oldBoard@D.Board{..} = 
    oldBoard{D.waiting=newWaiting, D.futile=newFutile:futile, D.seen=seen++defeaterSeen} 

{- 8 
Given 'lucky' and 'waiting' are all empty.  
Then all defeater are warranted. In this case
    - convert futile :: [(Path,Defeater)] to Node [(Path,Defeater)]
-}
unwarranted ::(Show a) =>  D.SearchRecords a -> D.Defeater a 
unwarranted = D.Unwarranted 

-- {- 9 
-- Given that the PathRecord related defeater is unwarranted. We have a new SearchReacord
--     - Remove old PathRecord from 'waiting' and get a new Board
--     - Return the new SearRecord 
--     - go back to 4 (new SearRecord as input)
-- -}
survived :: D.SearchRecord a-> D.PathRecords a-> D.Language a -> D.Board a-> D.Board a
survived newLuckyer newWaiting defeaterSeen board@D.Board{..}=
    board {D.waiting=newWaiting, D.lucky =newLuckyer:lucky, D.seen=seen++defeaterSeen}
