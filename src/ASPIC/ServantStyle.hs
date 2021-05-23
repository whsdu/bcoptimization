{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module ASPIC.ServantStyle where 

import Data.Kind (Type, Constraint)

data (a::k1) :|> (b ::k2)               -- PolyKinds : kind polymorphism    -- Type could have Zero data constructor 
infixr 5 :|> 

data Ha a b                             -- PolyKinds : (:k Ha :: k1 -> k2 -> *) , no PolyKinds ( :k Ha :: * -> * -> *)


type family F1 a 
type family F4 (a :: k1) :: k2 

-- data Haa Int 

type Exa a b = a :|> b

type Exp a = a -> Type 

data K = K1 | K2 | K3                   -- DataKinds to promote K1 to type level (compile time Data)
data L = L1 | L2 | L3 

type (:>|>) :: K -> L -> Type                           -- Stand alone kind signature ,  (:>|>) is a type constructor, logic connectives which takes proposition of kind K and L 
data (:>|>) a b 

type A :: (Type -> Type ) -> K -> Constraint            -- A 代表 Axiom， 其 可以配合 standalonekindsignatures
class A f k  | f -> k where 
    type TF (f :: * -> *) k ( m :: * -> *) :: *         -- TF f k m 可以认为是 instance 里具体声明的 alias
    e1 :: f String -> m Int -> TF f k m                 -- 没有  f -> k 的话， e1 无法推断出 k 是什么， 1） kind K 无法存在在run time， 所以必须提供 , 加上 f -> k 就可以推断了
    e2 :: f Int -> m Bool -> Char 

f1 :: a -> b -> Char 
f1 x y = 'w'

instance A Maybe K1 where 
    type TF Maybe K1 m = m Char                          -- 在这个Instance 里 type TF Maybe K1 IO 就是 Char 的 alias
    e1 = undefined 
    e2 = undefined 

data Nat = Zero | Succ Nat     

type (>=) :: Nat -> Nat -> Constraint 
class m >= n 
instance m >= Zero
instance (m >= n) => (Succ m >= Succ n)
instance {-# OVERLAPPABLE #-}(m >= n) => (Succ m >= n) 
