module ASPIC.Default where 

import ASPIC.AS (Literal(..),name)
import ASPIC.Abstraction(Negation(..))


instance Eq (Literal a) where
    (==) l1 l2 = name l1 == name l2

instance Negation (Literal a) where
    neg (Rule n b i h)
        |  head n == '!' =
            let nLiteral = tail n 
            in Rule nLiteral b i h
        | otherwise =
            let nLiteral = '!' : n
            in Rule nLiteral b i h
    neg (Atom n a)
        |  head n  == '!' =
            let nLiteral = tail n
            in Atom nLiteral a
        | otherwise =
            let nLiteral = '!' : n
            in Atom nLiteral a

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