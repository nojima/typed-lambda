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

unify :: Type -> Type -> TypeChecker ()
unify type1 type2 =
    if type1 == type2 then
        return ()
    else
        case (type1, type2) of
            (Type.Variable v1, _) | doesNotOccur v1 type2 -> do
                updateEnv $ substituteEnv v1 type2
                updateEnv $ Map.insert v1 type2
            (_, Type.Variable v2) | doesNotOccur v2 type1 -> do
                updateEnv $ substituteEnv v2 type1
                updateEnv $ Map.insert v2 type1
            (Type.Function arg1 ret1, Type.Function arg2 ret2) -> do
                unify arg1 arg2
                ret1' <- substituteTypeByEnv <$> getEnv <*> pure ret1
                ret2' <- substituteTypeByEnv <$> getEnv <*> pure ret2
                unify ret1' ret2'
            _ ->
                let
                    errorMessage =
                        "unification failed: "
                        <> "type1 = "
                        <> Type.pretty type1
                        <> ", type2 = "
                        <> Type.pretty type2
                in
                Except.throwError $ TypeError errorMessage

doesNotOccur :: Identifier -> Type -> Bool
doesNotOccur identifier type_ =
    case type_ of
        Type.Function arg ret ->
            doesNotOccur identifier arg || doesNotOccur identifier ret
        Type.Variable identifier_ ->
            identifier /= identifier_
        _ -> True

getEnv :: TypeChecker Environment
getEnv =
    stateEnv <$> State.get

updateEnv :: (Environment -> Environment) -> TypeChecker ()
updateEnv f = do
    state <- State.get
    let newEnv = f (stateEnv state)
    State.put $ state { stateEnv = newEnv }

withNestedEnv :: Identifier -> Type -> TypeChecker a -> TypeChecker a
withNestedEnv name type_ action = do
    -- Keep old type before update
    old <- Map.lookup name <$> getEnv

    -- Update env, and then call the action
    updateEnv $ Map.insert name type_
    result <- action

    -- Restore env
    updateEnv $ Map.update (const old) name

    return result

substituteTypeByEnv :: Environment -> Type -> Type
substituteTypeByEnv env type_ =
    case type_ of
        Type.Bool ->
            type_
        Type.Int ->
            type_
        Type.Function arg ret ->
            Type.Function (substituteTypeByEnv env arg) (substituteTypeByEnv env ret)
        Type.Variable identifier ->
            case Map.lookup identifier env of
                Nothing ->
                    type_
                Just t ->
                    substituteTypeByEnv env t

substituteType :: Identifier -> Type -> Type -> Type
substituteType identifier type_ target =
    case target of
        Type.Function arg ret ->
            Type.Function
                (substituteType identifier type_ arg)
                (substituteType identifier type_ ret)
        Type.Variable identifier_ | identifier_ == identifier ->
            type_
        _ ->
            target

substituteEnv :: Identifier -> Type -> Environment -> Environment
substituteEnv identifier type_ =
    Map.map (substituteType identifier type_)

newSymbol :: TypeChecker Identifier
newSymbol = do
    state <- State.get
    let nextId = stateNextId state
    let identifier = Identifier.Identifier $ T.pack $ 't' : show nextId
    State.put $ state { stateNextId = nextId + 1 }
    return identifier

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
typeOfIf _ condTerm thenTerm elseTerm = do
    condType <- typeOf condTerm
    unify Type.Bool condType

    thenType <- typeOf thenTerm
    elseType <- typeOf elseTerm
    unify thenType elseType

    substituteTypeByEnv <$> getEnv <*> pure thenType

typeOfLambda :: SourcePos -> Identifier -> Term -> TypeChecker Type
typeOfLambda _ argumentName body = do
    argumentType <- Type.Variable <$> newSymbol
    bodyType <- withNestedEnv argumentName argumentType (typeOf body)
    let functionType_ = Type.Function argumentType bodyType
    substituteTypeByEnv <$> getEnv <*> pure functionType_

typeOfApply :: SourcePos -> Term -> Term -> TypeChecker Type
typeOfApply pos function argument = do
    argumentType <- typeOf argument
    functionType <- typeOf function

    symbol <- newSymbol
    let retType = Type.Variable symbol
    unify (Type.Function argumentType retType) functionType

    substituteTypeByEnv <$> getEnv <*> pure retType

mustBeInt :: SourcePos -> Text -> Term -> TypeChecker ()
mustBeInt pos hint term = do
    type_ <- typeOf term
    unify Type.Int type_

mustBeBool :: SourcePos -> Text -> Term -> TypeChecker ()
mustBeBool pos hint term = do
    type_ <- typeOf term
    unify Type.Bool type_

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
            unify lhsType rhsType
            return Type.Bool

typeOfLet :: SourcePos -> Identifier -> Term -> Term -> TypeChecker Type
typeOfLet _ name expr body = do
    type_ <- typeOf expr
    withNestedEnv name type_ (typeOf body)

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

        Term.Lambda pos argumentName body ->
            typeOfLambda pos argumentName body

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
        TypingState { stateNextId = 1, stateEnv = Map.empty }
