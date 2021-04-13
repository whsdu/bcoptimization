module ASPIC.DefeasibleSpec where 

import Test.Hspec
import Control.Exception (evaluate)
import Test.QuickCheck

import qualified ASPIC.Defeasible as D 
import Data.Numbers.Primes ( isPrime, primes )

spec :: Spec 
spec = 
    describe "These are QuickCheck tests" $ do 
        context "This is a test of context" $ do 
            it "test one" $ 
                head [23..] `shouldBe` (23 :: Int)
            it "test two" $ 
                tail [1,2,3] `shouldBe` ([2,3] :: [Int])
            it "exception detected" $
                evaluate( head [] ) `shouldThrow` anyException 
        context "this is a test of quickCheck" $  do 
            it "== works" $ property $ equ 0
            it "list property v1" $ property prop'index'v1
            it "list property v3" $ property prop'index'v3
            it "list property v4" $ property prop'index'v4
            it "primes property v1" $ property prop_PrimeSum_v1'
            it "primes property v2" $ property prop'PrimeSum'v2
            -- it "primes property v3" $ property prop'PrimeSum'v3      -- computational expensive 
            it "primes property v4" $ property prop'PrimeSum'v4
            

equ :: Int -> Property  
equ  x = forAll (choose (0,0 :: Int) ) $ \y -> y==x 

prop'index'v1 :: [Integer] -> Int -> Bool 
prop'index'v1 xs n = xs !! n == head (drop n xs) 

prop'index'v3 ::NonEmptyList Integer -> NonNegative Int -> Property 
prop'index'v3 (NonEmpty xs) (NonNegative n) = 
    n < length xs ==> xs !! n == head (drop n xs) 

prop'index'v4 ::NonEmptyList Integer -> Property 
prop'index'v4 (NonEmpty xs)  = 
    forAll (choose (0,length xs -1)) $ \n ->  xs !! n == head (drop n xs) 

prop_PrimeSum_v1' :: Int -> Int -> Property
prop_PrimeSum_v1' p q =
  p > 2 && q > 2 && isPrime p && isPrime q ==>
  classify (p < 20 && q < 20) "trivial" $ even (p + q)

prop'PrimeSum'v2 :: Positive (Large Int)-> Positive (Large Int)-> Property
prop'PrimeSum'v2 (Positive (Large p)) (Positive (Large q)) =
  p > 2 && q > 2 && isPrime p && isPrime q ==>
  collect (if p < q then (p,q) else (q,p)) $ even (p + q)

prop'PrimeSum'v3 ::  Property
prop'PrimeSum'v3  =
    forAll (choose (1,1000)) $ \i -> 
        forAll (choose (1,1000)) $ \j -> 
            let (p,q) = (primes !! i, primes !! j) in 
                collect (if p < q then (p,q) else (q,p)) $ even (p + q) 

newtype Prime a = Primes a deriving Show 

instance (Integral a, Arbitrary a) => Arbitrary (Prime a) where
    arbitrary = do 
        x <- frequency [ (10, choose (0,1000))
                        , (5, choose(1001,10000))
                        , (1, choose( 10001, 50000))
                        ] 
        pure $ Primes (primes !! x) 


prop'PrimeSum'v4 :: Prime Int -> Prime Int ->   Property
prop'PrimeSum'v4  (Primes p) (Primes q)=
    p > 2 && q > 2 ==> 
        classify ( p < 1000 && q < 1000) "small primes" $ 
            classify ( p > 1000 || q > 1000) "medium primes" $ 
                classify (p > 1000 && q > 1000) "large primes" $ even (p + q) 