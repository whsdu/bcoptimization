module ASPIC.DefeasibleSpec where 

import Test.Hspec
import Control.Exception (evaluate)

spec :: Spec 
spec = 
    describe "This is the first test" $ do 
        context "This is a test of context" $ do 
            it "test one" $ 
                head [23..] `shouldBe` (23 :: Int)
            it "test two" $ 
                tail [1,2,3] `shouldBe` ([2,3] :: [Int])
            it "exception detected" $
                evaluate( head [] ) `shouldThrow` anyException 
