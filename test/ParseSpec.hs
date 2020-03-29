{-# LANGUAGE OverloadedStrings #-}
module ParseSpec where

import           Parse
import           Term (Term)
import qualified Term
import qualified Type
import           Test.Hspec
import qualified Data.Either as Either

spec :: Spec
spec =
    describe "parse" $ do
        context "bool literal" $ do
            it "can parse `true`" $
                parse "true" `shouldBe` Right (Term.Bool True)

            it "can parse `false`" $
                parse "false" `shouldBe` Right (Term.Bool False)

            it "can parse `true` with trailing space" $
                parse "true  " `shouldBe` Right (Term.Bool True)

            it "can parse `false` with trailing space" $
                parse "false \n" `shouldBe` Right (Term.Bool False)

        context "nat literal" $ do
            it "can parse `0`" $
                parse "0" `shouldBe` Right (Term.Nat 0)

            it "can parse `10`" $
                parse "10" `shouldBe` Right (Term.Nat 10)

            it "can parse `0` with trailing space" $
                parse "0 " `shouldBe` Right (Term.Nat 0)

            it "cannot parse `-1`" $
                shouldBeError (parse "-1")

        context "lambda expression" $ do
            it "can pares lambda expression" $
                parse "lambda x : Bool . foo" `shouldBe`
                    Right (Term.Lambda "x" Type.Bool (Term.Variable "foo"))

            it "can apply lambda" $
                parse "(lambda x : Bool . x) 10" `shouldBe`
                    Right (Term.Apply
                        (Term.Lambda "x" Type.Bool (Term.Variable "x"))
                        (Term.Nat 10)
                    )

            it "can apply multiple argument" $
                parse "(lambda x : Bool . lambda y : Nat . y) true 10" `shouldBe`
                    Right (Term.Apply
                        (Term.Apply
                            (Term.Lambda "x" Type.Bool
                                (Term.Lambda "y" Type.Nat
                                    (Term.Variable "y")
                                )
                            )
                            (Term.Bool True)
                        )
                        (Term.Nat 10)
                    )

        context "if expression" $
            it "can parse if expression" $
                parse "if true then 1 else 2" `shouldBe`
                    Right (Term.If (Term.Bool True) (Term.Nat 1) (Term.Nat 2))

        context "variable" $ do
            it "can parse variable" $
                parse "x" `shouldBe` Right (Term.Variable "x")

            it "distinguishes keyword from variables" $
                shouldBeError (parse "then")

shouldBeError :: Either String Term -> Expectation
shouldBeError result =
    result `shouldSatisfy` Either.isLeft
