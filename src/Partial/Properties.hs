module Partial.Properties where 

import Prelude hiding (negate)
import Control.Monad (guard)
import Data.List (notElem)
import qualified Data.HashMap.Strict as Map 

import qualified Partial.Argument as A 
{-
could be abstracted as env
now using classical negative
-}
negate :: A.Proposition -> A.Proposition 
negate (A.Proposition t c) 
    |  head c == '!' =
        let new'content = tail c
        in A.Proposition t c 
    | otherwise =
        let new'content = '!' : c
        in A.Proposition t c 

isNegated :: A.Proposition -> A.Proposition -> Bool 
isNegated p1 p2@(A.Proposition _ c2) = 
    let 
        A.Proposition _ c = negate p1 
    in 
        A.Proposition "" c == A.Proposition "" c2 

conC :: A.Argument  -> A.Proposition  
conC (A.L'Argument p) = p 
conC (A.P'Argument c _ _ ) = c



-- | TODO: the name of the rule should be check from the Context 
topRule :: A.Argument -> A.Rule
topRule (A.L'Argument p) = A.Rule "" p A.Undecided [] 
topRule (A.P'Argument p tr subs) = A.Rule "" p tr $ conC <$> subs 

getRuleName :: A.Context -> A.Rule -> Maybe A.Name 
getRuleName c r = 
        if null rls' then Nothing 
        else 
            Just $ A.name . head $ rls'
        where 
            rls = A.rules c 
            rls' = do 
                    r' <- rls
                    guard $ r' == r
                    pure r'

{--}
sub :: A.Argument -> [A.Argument]
sub a@(A.L'Argument _) = [a]
sub (A.P'Argument _ _ as) = concat $ sub <$> as 

{--}
prem :: A.Argument  -> [A.Rule]
prem (A.L'Argument p) = [A.Rule "" p A.Undecided []]
prem (A.P'Argument p imp body)  
    | null body = [A.Rule "" p imp []]  
    | otherwise  = concat $ prem <$> body 

-- | TODO: 
-- This recursive definition is not efficient enough.
-- How could this return will be more efficient ? 
comp :: A.Argument -> [A.Proposition]
comp (A.L'Argument p) = [p]
comp (A.P'Argument p imp body) 
    | null body = [] 
    | otherwise = concat $ comp <$> body

expandable :: A.Context -> A.Argument -> A.Expandability 
expandable context a
    | null $ comp a = A.Complete 
    | or (flip notElem (A.head <$> rls) <$> comp a) = A.Pending 
    | otherwise = A.Expandable 
    where 
        rls = A.rules context 