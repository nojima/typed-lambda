{-# LANGUAGE OverloadedStrings #-}
module TypeCheck (typeCheck, TypeError(..)) where

import           Term (Term, Operator, SourcePos)
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

mustBeNat :: Context -> SourcePos -> Text -> Term -> Either TypeError ()
mustBeNat context pos hint term = do
    type_ <- typeOf context term
    case type_ of
        Type.Nat ->
            Right ()
        other ->
            let
                errorMessage =
                    T.pack (Term.sourcePosPretty pos)
                    <> ": "
                    <> hint
                    <> " must be a natural number, but actually "
                    <> Type.pretty other
            in
            Left $ TypeError errorMessage

mustBeBool :: Context -> SourcePos -> Text -> Term -> Either TypeError ()
mustBeBool context pos hint term = do
    type_ <- typeOf context term
    case type_ of
        Type.Bool ->
            Right ()
        other ->
            let
                errorMessage =
                    T.pack (Term.sourcePosPretty pos)
                    <> ": "
                    <> hint
                    <> " must be a boolean, but actually "
                    <> Type.pretty other
            in
            Left $ TypeError errorMessage

typeOfBinOp :: Context -> SourcePos -> Operator -> Term -> Term -> Either TypeError Type
typeOfBinOp context pos operator lhs rhs =
    case operator of
        Term.Add -> do
            mustBeNat context pos "the 1st argument of add operator" lhs
            mustBeNat context pos "the 2nd argument of add operator" rhs
            return Type.Nat

        Term.Sub -> do
            mustBeNat context pos "the 1st argument of sub operator" lhs
            mustBeNat context pos "the 2nd argument of sub operator" rhs
            return Type.Nat

        Term.Mul -> do
            mustBeNat context pos "the 1st argument of mul operator" lhs
            mustBeNat context pos "the 2nd argument of mul operator" rhs
            return Type.Nat

        Term.Div -> do
            mustBeNat context pos "the 1st argument of div operator" lhs
            mustBeNat context pos "the 2nd argument of div operator" rhs
            return Type.Nat

        Term.And -> do
            mustBeBool context pos "the 1st argument of and operator" lhs
            mustBeBool context pos "the 2nd argument of and operator" rhs
            return Type.Bool

        Term.Or -> do
            mustBeBool context pos "the 1st argument of or operator" lhs
            mustBeBool context pos "the 2nd argument of or operator" rhs
            return Type.Bool

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

        Term.BinOp pos operator lhs rhs ->
            typeOfBinOp context pos operator lhs rhs

typeCheck :: Term -> Either TypeError Type
typeCheck term =
    let
        rootContext = Context (Identifier.Identifier "") Type.Bool Nothing
    in
    typeOf rootContext term
