{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}

module ASPIC.Default where

import           ASPIC.Abstraction   (CheckNegationFunction, DefeaterSelection,
                                      NegationFunction, OrderFunction,
                                      PathSelection)
import qualified ASPIC.Defeasible    as D

import qualified Data.HashMap.Strict as Map
import           Data.Maybe          (fromMaybe)

import           Data.Function       (on)
import           Data.List           (sortBy)
-- instance Eq (Literal ()) where
--     (==) l1 l2 = name l1 == name l2

neg :: NegationFunction ()
neg (D.Rule n a i h)
    |  head n == '!' =
        let nLiteral = tail n
        in D.Rule nLiteral a i h
    | otherwise =
        let nLiteral = '!' : n
        in D.Rule nLiteral a i h
neg (D.Atom n a)
    |  head n  == '!' =
        let nLiteral = tail n
        in D.Atom nLiteral a
    | otherwise =
        let nLiteral = '!' : n
        in D.Atom nLiteral a

isNegation :: CheckNegationFunction ()
isNegation = (==)

selectionOne :: PathSelection a
selectionOne srs =
        let
            sortByLength = sortBy (flip compare `on` (length . fst)) srs
            sortByDefeasible = sortBy (flip compare `on` (length . getDef . fst)) srs
        in (head sortByDefeasible, tail sortByDefeasible)
    where
        getDef :: D.Argument a-> D.Language a
        getDef p =
            let
                rules = concat p
            in [r | r<-rules, D.imp r == D.D]

selectionTwo :: DefeaterSelection a
selectionTwo prs =
        let
            sortByLength = sortBy (flip compare `on` (length . fst)) prs
            sortByDefeasible = sortBy (flip compare `on` (length . getDef . fst)) prs
        in (head sortByDefeasible, tail sortByDefeasible)
    where
        getDef :: D.Argument a-> D.Language a
        getDef p =
            let
                rules = concat p
            in [r | r<-rules, D.imp r == D.D]

-- | here could provide complicate implementation
ordering :: Eq a=> OrderFunction a
ordering prefMap attacker defender =
    -- ord prefMap (concat attacker) (concat defender)
    let
        defenderSet = lastLinkChecker defender (D.conC <$> head defender)
        attackerSet = lastLinkChecker attacker (D.conC <$> head attacker)
    in ord prefMap attackerSet defenderSet
    where
        lastLinkChecker ::(Eq a) => D.Argument a-> D.Language a-> D.Language a
        lastLinkChecker p targets =
            let
                rules = concat p
            in fromMaybe [] (lastLinkScan rules targets)

        lastLinkScan :: (Eq a) => D.Language a-> D.Language a-> Maybe (D.Language a)
        lastLinkScan _ [] = Just []
        lastLinkScan rules targets = do
            let
                rs = [r | r <- rules, D.conC r `elem` targets]
            if length rs /= length targets
                    then Nothing
                    else
                        let
                            strictLine = [ r | r <- rules, D.conC r `elem` targets, D.imp r == D.S, (not . null) (D.body r)]
                            defeasible = [ r | r <- rules, D.conC r `elem` targets, D.imp r == D.D, (not . null) (D.body r)]
                            premise = [ r|  r <- rules, D.conC r `elem` targets, null (D.body r)]
                        in do
                            rlist <- lastLinkScan rules (concat (D.body <$> strictLine))
                            pure $ rlist ++ defeasible ++ premise
ord :: Eq a => D.PreferenceMap -> D.Language a -> D.Language a-> Bool
ord pm attackerDefs argDefs
    | null ldrA && null ldrB = eli pm (axiA ++ ordiA) (axiB ++ ordiB)
    | null ldrA = True
    | otherwise = eli pm ldrA ldrB
    where
        ldrA = rulesToDefs attackerDefs
        ldrB = rulesToDefs argDefs
        axiA = rulesToPrems attackerDefs
        axiB = rulesToPrems argDefs
        ordiA = rulesToAxiom attackerDefs
        ordiB = rulesToAxiom argDefs

rulesToDefs :: D.Language a-> D.Language a
rulesToDefs rules = [r | r<-rules, D.imp r == D.D, (not . null) (D.body r)]

rulesToPrems :: D.Language a-> D.Language a
rulesToPrems rules = [r | r<-rules, D.imp r == D.D , null (D.body r)]

rulesToAxiom:: D.Language a -> D.Language a
rulesToAxiom rules = [r | r<-rules, D.imp r == D.S , null (D.body r)]

rulesToLDR::Eq a=> D.Language a -> D.Language a
rulesToLDR rules = getLDR rules (head rules)

getLDR :: Eq a => D.Language a -> D.Literal a-> D.Language a
getLDR rules r
    | D.imp r == D.D = [r]
    | otherwise =  concat $ getLDR rules <$> [subr | subr <- rules, D.conC subr `elem` D.body r ]

eli :: D.PreferenceMap -> D.Language a-> D.Language a-> Bool
eli pMap argA argB
        | null argA && null argB = False
        | null argB = False
        | null argA  =  True
        | otherwise = eli' pMap argA argB

eli' :: D.PreferenceMap -> D.Language a-> D.Language a-> Bool
eli' pMap l1 (l:ls)=
    let
        rl = [sl | sl <- l1 , preferThan pMap sl l]
    in
        ((length rl == length l1) || eli' pMap l1 ls)
eli' pMap l1 [] = False

preferThan pMap l1 l2 =
    let cMay = do
                r1 <- Map.lookup (D.name l1) pMap
                r2 <- Map.lookup (D.name l2) pMap
                pure $ r1 >= r2
    in Just True == cMay

-- lanEqual :: Language -> Language -> Bool
-- lanEqual al bl = isElemB && isElemA
--     where
--         isElemB = and [ a `elem` bl | a <- al ]
--         isElemA = and [ b `elem` al | b <- bl ]

-- instance (Show a) => Show (Literal a) where
--     show (Rule n b i h) = n ++ ": " ++ bs ++ im ++ head
--         where
--             bs = unwords $ name <$> b
--             im = show i
--             head = name h
--     show (Atom n) = n

-- instance Eq Literal where
--     (==) l1 l2 = name l1 == name l2

-- instance Ord Literal where
--     compare l1 l2 = compare (name l1) (name l2 )

-- -- | By default : negation a1 a2 = neg a1 == a2
-- instance Negation Literal where
--     neg (Rule n b i h)
--         |  GHC.head n == '!' =
--             let nLiteral = tail n
--             in Rule nLiteral b i h
--         | otherwise =
--             let nLiteral = '!' : n
--             in Rule nLiteral b i h
--     neg (Atom n)
--         |  GHC.head n  == '!' =
--             let nLiteral = tail n
--             in Atom nLiteral
--         | otherwise =
--             let nLiteral = '!' : n
--             in Atom nLiteral

-- newtype AnonyRule = AnonyRule {unanonyRule :: Literal}

-- instance Eq AnonyRule where
--     (==) aR1 aR2
--         | imp  r1 /= imp r2 = False
--         | conC r1 /= conC r2 = False
--         | body r1 /= body r2 = False
--         | otherwise = True
--         where
--             r1 = unanonyRule aR1
--             r2 = unanonyRule aR2

-- instance Show AnonyRule where
--     show ar = show $ unanonyRule ar
