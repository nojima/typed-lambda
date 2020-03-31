{-# LANGUAGE OverloadedStrings #-}
module TypeCheck (typeCheck, TypeError(..)) where

import           Term (Term, SourcePos)
import qualified Term
import           Type (Type)
import qualified Type
import           Identifier (Identifier)
import qualified Identifier
import           Data.Text (Text)
import qualified Data.Text as T

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

typeOfVariable :: Context -> SourcePos -> Identifier -> Either TypeError Type
typeOfVariable context pos identifier =
    case lookupVariable identifier context of
        Nothing ->
            let
                errorMessage =
                    T.pack (Term.sourcePosPretty pos)
                    <> ": undefined variable '"
                    <> Identifier.name identifier
                    <> "'"
            in
            Left $ TypeError errorMessage

        Just type_ ->
            Right type_

typeOfIf :: Context -> SourcePos -> Term -> Term -> Term -> Either TypeError Type
typeOfIf context pos condTerm thenTerm elseTerm = do
    condType <- typeOf context condTerm
    thenType <- typeOf context thenTerm
    elseType <- typeOf context elseTerm

    if condType /= Type.Bool then
        let
            errorMessage =
                T.pack (Term.sourcePosPretty pos)
                <> ": non-bool '"
                <> Type.pretty condType
                <> "' used as `if` condition"
        in
        Left $ TypeError errorMessage

    else if thenType /= elseType then
        let
            errorMessage =
                T.pack (Term.sourcePosPretty pos)
                <> ": `then` and `else` have incompatible types.\n"
                <> "  then: "
                <> Type.pretty thenType
                <> "\n"
                <> "  else: "
                <> Type.pretty elseType
                <> "\n"
        in
        Left $ TypeError errorMessage

    else
        Right thenType

typeOfLambda :: Context -> Identifier -> Type -> Term -> Either TypeError Type
typeOfLambda context argumentName argumentType body =
    let
        newContext = Context argumentName argumentType (Just context)
    in
    Type.Function argumentType <$> typeOf newContext body

typeOfApply :: Context -> SourcePos -> Term -> Term -> Either TypeError Type
typeOfApply context pos function argument = do
    functionType <- typeOf context function
    argumentType <- typeOf context argument

    case functionType of
        Type.Function expectedArgumentType bodyType ->
            if expectedArgumentType == argumentType then
                Right bodyType
            else
                let
                    errorMessage =
                        T.pack (Term.sourcePosPretty pos)
                        <> ": function argument has a incompatible type:\n"
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
                    T.pack (Term.sourcePosPretty pos)
                    <> ": cannot apply non-function '"
                    <> Type.pretty type_
                    <> "'"
            in
            Left $ TypeError errorMessage

typeOf :: Context -> Term -> Either TypeError Type
typeOf context term =
    case term of
        Term.Bool _ _ ->
            Right Type.Bool

        Term.Nat _ _ ->
            Right Type.Nat

        Term.If pos condTerm thenTerm elseTerm ->
            typeOfIf context pos condTerm thenTerm elseTerm

        Term.Variable pos identifier ->
            typeOfVariable context pos identifier

        Term.Lambda _ argumentName argumentType body ->
            typeOfLambda context argumentName argumentType body

        Term.Apply pos function argument ->
            typeOfApply context pos function argument

typeCheck :: Term -> Either TypeError Type
typeCheck term =
    let
        rootContext = Context (Identifier.Identifier "") Type.Bool Nothing
    in
    typeOf rootContext term
