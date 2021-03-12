{-# LANGUAGE MultiParamTypeClasses #-}
module ASPIC.Abstraction where 

import ASPIC.AS (Board)

import Data.List (group, sort)


class (Eq a) => Negation a where 
    neg :: a -> a 
    negation :: a -> a -> Bool 
    negation a1 a2 = neg a1 == a2

type SelectionFunction =  Board -> Bool 