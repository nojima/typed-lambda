{-# LANGUAGE OverloadedStrings #-}
module ParseSpec where

import           Parse
import           Term (Term)
import qualified Term
import qualified Text.Megaparsec.Pos as ParsecPos
import           Test.Hspec
import qualified Data.Either as Either

-- dummy source pos
x :: Term.SourcePos
x = ParsecPos.initialPos ""

spec :: Spec
spec =
    describe "parse" $ do
        context "bool literal" $ do
            it "can parse `true`" $
                parse "true" `shouldSuccess` Term.Bool x True

            it "can parse `false`" $
                parse "false" `shouldSuccess` Term.Bool x False

            it "can parse `true` with trailing space" $
                parse "true  " `shouldSuccess` Term.Bool x True

            it "can parse `false` with trailing space" $
                parse "false \n" `shouldSuccess` Term.Bool x False

        context "nat literal" $ do
            it "can parse `0`" $
                parse "0" `shouldSuccess` Term.Int x 0

            it "can parse `10`" $
                parse "10" `shouldSuccess` Term.Int x 10

            it "can parse `0` with trailing space" $
                parse "0 " `shouldSuccess` Term.Int x 0

            it "cannot parse `-1`" $
                shouldBeError (parse "-1")

        context "lambda expression" $ do
            it "can pares lambda expression" $
                parse "lambda hoge . foo" `shouldSuccess`
                    Term.Lambda x "hoge" (Term.Variable x "foo")

            it "can apply lambda" $
                parse "(lambda hoge . hoge) 10" `shouldSuccess`
                    Term.Apply x
                        (Term.Lambda x "hoge" (Term.Variable x "hoge"))
                        (Term.Int x 10)

            it "can apply multiple argument" $
                parse "(lambda hoge . lambda fuga . fuga) true 10" `shouldSuccess`
                    Term.Apply x
                        (Term.Apply x
                            (Term.Lambda x "hoge"
                                (Term.Lambda x "fuga"
                                    (Term.Variable x "fuga")
                                )
                            )
                            (Term.Bool x True)
                        )
                        (Term.Int x 10)

        context "if expression" $
            it "can parse if expression" $
                parse "if true then 1 else 2" `shouldSuccess`
                    Term.If x (Term.Bool x True) (Term.Int x 1) (Term.Int x 2)

        context "variable" $ do
            it "can parse variable" $
                parse "v" `shouldSuccess` Term.Variable x "v"

            it "distinguishes keyword from variables" $
                shouldBeError (parse "then")

shouldSuccess :: Either String Term -> Term -> Expectation
shouldSuccess actual expected =
    case actual of
        Right term ->
            (Term.mapSourcePos (const x) term) `shouldBe` expected
        Left _ ->
            actual `shouldBe` (Right expected)

shouldBeError :: Either String Term -> Expectation
shouldBeError result =
    result `shouldSatisfy` Either.isLeft
