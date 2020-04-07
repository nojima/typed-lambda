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
import qualified Data.Map.Strict as Map
import qualified Control.Monad.State.Strict as State
import qualified Control.Monad.Except as Except

newtype TypeError = TypeError Text

type Environment = Map.Map Identifier Type

data TypingState = TypingState
    { stateNextId :: !Int
    , stateEnv :: Environment
    }
    deriving (Show, Eq)

type TypeChecker a = Except.ExceptT TypeError (State.State TypingState) a

getEnv :: TypeChecker Environment
getEnv =
    stateEnv <$> State.get

updateEnv :: (Environment -> Environment) -> TypeChecker ()
updateEnv f = do
    state <- State.get
    let newEnv = f (stateEnv state)
    State.put $ state { stateEnv = newEnv }

lookupVariable :: Identifier -> TypeChecker (Maybe Type)
lookupVariable identifier =
    Map.lookup identifier <$> getEnv

typeOfVariable :: SourcePos -> Identifier -> TypeChecker Type
typeOfVariable pos identifier = do
    maybeType <- lookupVariable identifier
    case maybeType of
        Nothing ->
            let
                errorMessage =
                    T.pack (Term.sourcePosPretty pos)
                    <> ": undefined variable '"
                    <> Identifier.name identifier
                    <> "'"
            in
            Except.throwError $ TypeError errorMessage

        Just type_ ->
            return type_

typeOfIf :: SourcePos -> Term -> Term -> Term -> TypeChecker Type
typeOfIf pos condTerm thenTerm elseTerm = do
    condType <- typeOf condTerm
    thenType <- typeOf thenTerm
    elseType <- typeOf elseTerm

    if condType /= Type.Bool then
        let
            errorMessage =
                T.pack (Term.sourcePosPretty pos)
                <> ": non-bool '"
                <> Type.pretty condType
                <> "' used as `if` condition"
        in
        Except.throwError $ TypeError errorMessage

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
        Except.throwError $ TypeError errorMessage

    else
        return thenType

validateUniqunessOfVariableName :: SourcePos -> Identifier -> TypeChecker ()
validateUniqunessOfVariableName pos identifier = do
    maybeType <- lookupVariable identifier
    case maybeType of
        Just _ ->
            let
                errorMessage =
                    T.pack (Term.sourcePosPretty pos)
                    <> ": the variable name must be different from each other. "
                    <> "This restriction will be eliminated in the future."
            in
            Except.throwError $ TypeError errorMessage
        Nothing ->
            return ()

typeOfLambda :: SourcePos -> Identifier -> Type -> Term -> TypeChecker Type
typeOfLambda pos argumentName argumentType body = do
    -- TODO: この制限を緩和する
    validateUniqunessOfVariableName pos argumentName

    updateEnv $ Map.insert argumentName argumentType
    bodyType <- typeOf body
    return $ Type.Function argumentType bodyType

typeOfApply :: SourcePos -> Term -> Term -> TypeChecker Type
typeOfApply pos function argument = do
    functionType <- typeOf function
    argumentType <- typeOf argument

    case functionType of
        Type.Function expectedArgumentType bodyType ->
            if expectedArgumentType == argumentType then
                return bodyType
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
                Except.throwError $ TypeError errorMessage

        type_ ->
            let
                errorMessage =
                    T.pack (Term.sourcePosPretty pos)
                    <> ": cannot apply non-function '"
                    <> Type.pretty type_
                    <> "'"
            in
            Except.throwError $ TypeError errorMessage

mustBeInt :: SourcePos -> Text -> Term -> TypeChecker ()
mustBeInt pos hint term = do
    type_ <- typeOf term
    case type_ of
        Type.Int ->
            return ()
        other ->
            let
                errorMessage =
                    T.pack (Term.sourcePosPretty pos)
                    <> ": "
                    <> hint
                    <> " must be an integer, but actually "
                    <> Type.pretty other
            in
            Except.throwError $ TypeError errorMessage

mustBeBool :: SourcePos -> Text -> Term -> TypeChecker ()
mustBeBool pos hint term = do
    type_ <- typeOf term
    case type_ of
        Type.Bool ->
            return ()
        other ->
            let
                errorMessage =
                    T.pack (Term.sourcePosPretty pos)
                    <> ": "
                    <> hint
                    <> " must be a boolean, but actually "
                    <> Type.pretty other
            in
            Except.throwError $ TypeError errorMessage

typeOfBinOp :: SourcePos -> Operator -> Term -> Term -> TypeChecker Type
typeOfBinOp pos operator lhs rhs =
    case operator of
        Term.Add -> do
            mustBeInt pos "the 1st argument of `+` operator" lhs
            mustBeInt pos "the 2nd argument of `+` operator" rhs
            return Type.Int

        Term.Sub -> do
            mustBeInt pos "the 1st argument of `-` operator" lhs
            mustBeInt pos "the 2nd argument of `-` operator" rhs
            return Type.Int

        Term.Mul -> do
            mustBeInt pos "the 1st argument of `*` operator" lhs
            mustBeInt pos "the 2nd argument of `*` operator" rhs
            return Type.Int

        Term.Div -> do
            mustBeInt pos "the 1st argument of `/` operator" lhs
            mustBeInt pos "the 2nd argument of `/` operator" rhs
            return Type.Int

        Term.And -> do
            mustBeBool pos "the 1st argument of `&&` operator" lhs
            mustBeBool pos "the 2nd argument of `&&` operator" rhs
            return Type.Bool

        Term.Or -> do
            mustBeBool pos "the 1st argument of `||` operator" lhs
            mustBeBool pos "the 2nd argument of `||` operator" rhs
            return Type.Bool

        Term.Equal -> do
            lhsType <- typeOf lhs
            rhsType <- typeOf rhs
            if lhsType /= rhsType then
                let
                    errorMessage =
                        T.pack (Term.sourcePosPretty pos)
                        <> ": cannot compare `"
                        <> Type.pretty lhsType
                        <> "` with `"
                        <> Type.pretty rhsType
                        <> "`"
                in
                Except.throwError $ TypeError errorMessage
            else
                return Type.Bool

typeOfLet :: SourcePos -> Identifier -> Term -> Term -> TypeChecker Type
typeOfLet pos name expr body = do
    -- TODO: この制限を緩和する
    validateUniqunessOfVariableName pos name

    type_ <- typeOf expr
    updateEnv $ Map.insert name type_
    typeOf body

typeOf :: Term -> TypeChecker Type
typeOf term =
    case term of
        Term.Bool _ _ ->
            return Type.Bool

        Term.Int _ _ ->
            return Type.Int

        Term.If pos condTerm thenTerm elseTerm ->
            typeOfIf pos condTerm thenTerm elseTerm

        Term.Variable pos identifier ->
            typeOfVariable pos identifier

        Term.Lambda pos argumentName argumentType body ->
            typeOfLambda pos argumentName argumentType body

        Term.Apply pos function argument ->
            typeOfApply pos function argument

        Term.BinOp pos operator lhs rhs ->
            typeOfBinOp pos operator lhs rhs

        Term.Let pos name expr body ->
            typeOfLet pos name expr body

typeCheck :: Term -> Either TypeError Type
typeCheck term =
    State.evalState
        (Except.runExceptT (typeOf term))
        TypingState { stateNextId = 0, stateEnv = Map.empty }
