{-# LANGUAGE MultiParamTypeClasses #-}
module ASPIC.Abstraction where 


import Data.List (group, sort)

class (Eq a) => Negation a where 
    neg :: a -> a 
    negation :: a -> a -> Bool 
    negation a1 a2 = neg a1 == a2
 
class (Negation a) => Attack a c where 
    conflict :: a -> a -> c 
