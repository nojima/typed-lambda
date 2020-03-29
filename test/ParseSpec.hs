{-# LANGUAGE OverloadedStrings #-}
module ParseSpec where

import           Parse
import qualified Term
import           Test.Hspec

spec :: Spec
spec =
    describe "Parse.parse" $
        context "bool literal" $ do
            it "successfully parse `true`" $
                parse "true" `shouldBe` Right (Term.Bool True)

            it "successfully parse `false`" $
                parse "false" `shouldBe` Right (Term.Bool False)

            it "successfully parse `true` with trailing space" $
                parse "true  " `shouldBe` Right (Term.Bool True)

            it "successfully parse `false` with trailing space" $
                parse "false \n" `shouldBe` Right (Term.Bool False)
