{-# LANGUAGE OverloadedStrings #-}
module Parser.FileParser where 

import Control.Monad(guard)
import System.IO 
import Data.List.Split (splitOn)
import  qualified Data.HashMap.Strict  as Map 
import Data.Maybe(fromJust, fromMaybe)
import Data.Text(isInfixOf,pack)

import qualified ASPIC.Defeasible  as D 
    ( Name, Literal (..), Language , Rules(..), Imp(..), LogicLanguage(..), Rules(..),
     PreferenceMap, name,body,imp,conC, getLogicLanguage, LiteralMap,PreferenceMap )

import ASPIC.Abstraction (AS(..), NegationFunction)
import qualified ASPIC.Default as Default (neg,isNegation, selectionOne, selectionTwo, ordering)
import ASPIC.Ordering
import Toolkits.Common(rmdups) 

-- | TODOS:
{-
1. The neg being used in chainingRule function should be better arranged, is it necessary to included it here ?
    The relation between negation and checknegation is quite tricky 
-}


-- | Transitional data type being used to bridge the gap between file and list of Literal. 
-- File contains lines of strings that represent rules. 
-- Target is list of terms of type Literal 
-- The process is : 
-- 1. lines of string --> list of Knowledge
-- 2. List of Knowledge --> List of Literal
data Knowledge = Knowledge 
    { ruleName :: String
    , premisesName :: [String]
    , impName :: String
    , conclusionName :: String
    , preferName :: String 
    }deriving Show 

type KnowledgeSpace = [Knowledge]

{-
TODOs:
Language space in env does not contains neg of rules. 
-}
parseDefaultEnv :: FilePath -> IO (AS ())
parseDefaultEnv filePath = do 
    k <- fileToKnowledge filePath
    prefMap <- fileToPrefMap filePath
    let 
        l = k2l k
        r = chainingRule l Default.neg 
    pure $ mkDefEnv r prefMap

-- fileToPrefMap :: FilePath -> IO (L.RdPrefMap, L.KnwlPrefMap)

-- stringToEnv :: String -> IO Env 
-- stringToEnv content = do 
--     k <- stringToKnowledge content 
--     (rdMap, knMap) <- stringToPrefMap content 
--     let 
--         l = k2l k 
--         r = chainingRule l 
--     pure $ mkEnv r rdMap knMap 

parseLiteralMap :: AS () -> Map.HashMap D.Name (D.Literal ())
parseLiteralMap as = 
    let 
        language = D.getLogicLanguage . asLanguage $ as 
    in Map.fromList $ zip (D.name <$> language ) language

-- parsePreferenceMap :: Env -> L.PreferenceMap
-- parsePreferenceMap env = 
--     let
--         rdMap = L.getRdPrefMap . envRdPrefMap $ env
--         knMap = L.getKnwlPrefMap . envKnwlPrefMap $ env 
--     in Map.union rdMap knMap 


parseQueryLiteral :: String -> Map.HashMap D.Name (D.Literal ())-> D.Literal ()
parseQueryLiteral qName lm = fromJust $ Map.lookup qName lm 

-- | TODOs: 

-- now we have function to generate env from file 
-- 1. need function to generation env from elm interface. 
-- 2. need to function to union two env and handle possible error, such as conflict of name. 
-- 3. Absolutely no error detection and handling functions. 

-- | Auxiliary function converting string to data type knowledge 


fileToKnowledge :: FilePath -> IO KnowledgeSpace
fileToKnowledge filePath = do
    handle <- openFile filePath  ReadMode
    contents <- hGetContents handle 
    pure $ parseWord <$> removeComment (lines contents)

stringToKnowledge :: String -> IO KnowledgeSpace 
stringToKnowledge content =  pure $ parseWord <$> removeComment (lines content)

parseWord :: String -> Knowledge
parseWord w = 
    let 
        [ruleName,ruleBody] = splitOn ":" w 
        impName 
            | '-' `elem` ruleBody = "->"
            | otherwise = "=>"
        [premies,conC] = splitOn impName ruleBody 
        premisesName = splitOn "," premies 
        [conclusionName,preferName] = splitOn "," conC
    in Knowledge ruleName premisesName impName conclusionName preferName


k2l :: KnowledgeSpace -> D.LiteralMap ()
k2l knowledges = constructLS knowledges Map.empty
    where 
        constructLS (k:ks) lsAcc = 
            let 
                concName = conclusionName k 
                priNames = premisesName k 
                rName = ruleName k 
                iName = impName k 
                (updateAtomAcc,primLiterals,concLiteral) = insertAtomsToLanguageSpace concName  priNames lsAcc
                updateRuleAcc = insertRuleToLanguageSpace rName iName primLiterals concLiteral updateAtomAcc
            in constructLS ks updateRuleAcc
        constructLS [] lsAcc  = lsAcc
        insertAtomsToLanguageSpace :: String -> [String] -> D.LiteralMap () -> (D.LiteralMap (), D.Language (), D.Literal ())
        insertAtomsToLanguageSpace concName priNames ls = 
            let 
                (accPrim, primLiterals) = foldr insertOneAtom (ls,[]) priNames 
                (accConc, concLiterals) = insertOneAtom concName (accPrim,[])
            in (accConc, primLiterals, head concLiterals)
            where insertOneAtom n (ll,lbs) = 
                            case Map.lookup n ll of 
                                Just b -> (ll, b:lbs)
                                Nothing -> 
                                    let newl = D.Atom n ()
                                    in (Map.insert n newl ll, newl:lbs)
        insertRuleToLanguageSpace 
            :: String
            -> String 
            -> D.Language ()
            -> D.Literal ()
            -> D.LiteralMap ()  
            -> D.LiteralMap () 
        insertRuleToLanguageSpace ruleName imp primies conclusion lspace =
            let 
                impSym = if imp == "->" then D.S else D.D 
                bodies = if head primies == D.Atom "" () then [] else primies 
                ruleLiteral = D.Rule ruleName bodies impSym conclusion 
            in Map.insert ruleName ruleLiteral lspace 

chainingRule :: D.LiteralMap () -> NegationFunction () -> D.LiteralMap () 
chainingRule knowledgeMap neg = 
    let 
        ruleList = [ km | km <- Map.toList knowledgeMap , (D.imp . snd) km == D.D || (D.imp . snd) km == D.S] 
        ruleMap = Map.fromList [ km | km <- Map.toList knowledgeMap , (D.imp . snd) km == D.D || (D.imp . snd) km == D.S] 
    in Map.fromList $ chaining ruleMap <$> ruleList
    where 
        chaining rm tp = 
            let
                key = fst tp 
                p = snd tp 
                name = D.name p 
                imp = D.imp p 
                body = D.body p 
                conC = D.conC p 
                newBody = searchRules rm neg <$> body 
                newConc = searchRules rm neg conC 
            in (key, D.Rule name newBody imp newConc)
        searchRules :: D.LiteralMap () -> NegationFunction () -> D.Literal () -> D.Literal ()
        searchRules rm neg l = 
            case l of 
                D.Rule {} -> l 
                D.Atom "" () -> l 
                D.Atom _ _ -> 
                    let 
                        name = D.name l 
                    in 
                        if head name == '!' 
                            then 
                                let 
                                    oName = tail name 
                                    rO = Map.lookup oName rm 
                                in case rO of 
                                    Nothing -> l 
                                    Just rule -> neg rule 
                            else 
                                let r = Map.lookup name rm 
                                in fromMaybe l r 

-- -- | 
-- -- 1. preference map of rules and premises are separated. 
-- -- 2. no record of strict rules because this is not part of any preference set selection methods (weakest-link or last-link).
-- -- TODOs: 
-- -- improve the separation method of rules and premises. 

fileToPrefMap :: FilePath -> IO D.PreferenceMap
fileToPrefMap filePath = do 
    handle <- openFile filePath  ReadMode
    contents <- hGetContents handle 
    let 
        records = removeComment( lines contents )
        premisesLines = [r | r <- records,(":=" `isInfixOf` pack r) || (":-" `isInfixOf` pack r)]
        rulesLines = [r | r <- records, r `notElem` premisesLines && not ("->" `isInfixOf` pack r)]
        rdMap =  Map.fromList $ parsePre <$> rulesLines
        kwMap =  Map.fromList $ parsePre <$> premisesLines
        prefMap = Map.union rdMap kwMap 
    pure prefMap 
  where 
      parsePre :: String -> (D.Name,Int)
      parsePre s = 
          let 
              name = head $ splitOn ":" s 
              pre = read . last $ splitOn "," s 
          in (name,pre)

-- stringToPrefMap :: String -> IO (L.RdPrefMap, L.KnwlPrefMap)
-- stringToPrefMap contents = do 
--     let 
--         records = removeComment( lines contents )
--         premisesLines = [r | r <- records,(":=" `isInfixOf` pack r) || (":-" `isInfixOf` pack r)]
--         rulesLines = [r | r <- records, r `notElem` premisesLines && not ("->" `isInfixOf` pack r)]
--         rdMap = L.RdPrefMap $ Map.fromList $ parsePre <$> rulesLines
--         kwMap = L.KnwlPrefMap $ Map.fromList $ parsePre <$> premisesLines
--     pure (rdMap,kwMap)
--   where 
--       parsePre :: String -> (M.Name,Int)
--       parsePre s = 
--           let 
--               name = head $ splitOn ":" s 
--               pre = read . last $ splitOn "," s
--           in (name,pre)

-- | Remove 
-- 1. # : comment line 
-- 2. ' ' : lines with empty char
-- 3. "" : lines with no char
removeComment :: [String] -> [String]
removeComment sl = [s | s<-sl ,'#' `notElem` s && ' ' `notElem` s, s /= ""] 

mkDefEnv :: D.LiteralMap () -> D.PreferenceMap -> AS ()
mkDefEnv lm pm = 
    let 
        rules = snd <$> Map.toList lm 
        atoms = rmdups [ l | l <- concat (D.body <$> rules) ++ (D.conC <$> rules), D.imp l == D.N]
        ls = rules ++atoms 
    in AS  
        (D.LogicLanguage ls)
        (D.Rules rules)
        pm 
        Default.selectionOne
        Default.selectionTwo
        Default.neg 
        Default.isNegation
        Default.ordering 
