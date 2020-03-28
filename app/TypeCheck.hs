{-# LANGUAGE OverloadedStrings #-}
module TypeCheck (typeCheck, TypeError(..)) where

import           Term (Term)
import qualified Term
import           Type (Type)
import qualified Type
import           Identifier (Identifier)
import qualified Identifier
import           Data.Text (Text)

newtype TypeError = TypeError Text

data Context = Context
    { variableName  :: Identifier
    , variableType  :: Type
    , parentContext :: Maybe Context
    }

lookupVariable :: Identifier -> Context -> Maybe Type
lookupVariable identifier context =
    if variableName context == identifier
        then Just $ variableType context
        else lookupVariable identifier =<< parentContext context

typeOfVariable :: Context -> Identifier -> Either TypeError Type
typeOfVariable context identifier =
    case lookupVariable identifier context of
        Nothing ->
            let
                errorMessage =
                    "undefined variable '"
                    <> Identifier.name identifier
                    <> "'"
            in
            Left $ TypeError errorMessage

        Just type_ ->
            Right type_

typeOfLambda :: Context -> Identifier -> Type -> Term -> Either TypeError Type
typeOfLambda context argumentName argumentType body =
    let
        newContext = Context argumentName argumentType (Just context)
    in
    Type.Function argumentType <$> typeOf newContext body

typeOfApply :: Context -> Term -> Term -> Either TypeError Type
typeOfApply context function argument = do
    functionType <- typeOf context function
    argumentType <- typeOf context argument

    case functionType of
        Type.Function expectedArgumentType bodyType ->
            if expectedArgumentType == argumentType then
                Right bodyType
            else
                let
                    errorMessage =
                        "type mismatch:\n"
                        <> "  expected: "
                        <> Type.pretty expectedArgumentType
                        <> "\n"
                        <> "  acutally: "
                        <> Type.pretty argumentType
                        <> "\n"
                in
                Left $ TypeError errorMessage

        type_ ->
            let
                errorMessage =
                    "cannot apply non-function '"
                    <> Type.pretty type_
                    <> "'"
            in
            Left $ TypeError errorMessage

typeOf :: Context -> Term -> Either TypeError Type
typeOf context term =
    case term of
        Term.Bool _ ->
            Right Type.Bool

        Term.Variable identifier ->
            typeOfVariable context identifier

        Term.Lambda argumentName argumentType body ->
            typeOfLambda context argumentName argumentType body

        Term.Apply function argument ->
            typeOfApply context function argument

typeCheck :: Term -> Either TypeError Type
typeCheck term =
    let
        rootContext = Context (Identifier.Identifier "") Type.Bool Nothing
    in
    typeOf rootContext term
