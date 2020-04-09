{-# LANGUAGE OverloadedStrings #-}
module TypeCheck (typeCheck, TypeError(..)) where

import           Term (Term, Operator, SourcePos)
import qualified Term
import           Type (Type)
import qualified Type
import           Identifier (Identifier)
import qualified Identifier
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Control.Monad.State.Strict as State
import qualified Control.Monad.Except as Except

newtype TypeError = TypeError T.Text

data TypingState = TypingState
    { stateNextId :: !Int }
    deriving (Eq, Show)

type TypeChecker a = Except.ExceptT TypeError (State.State TypingState) a

-- 型環境
-- 変数名からその型へのマップ。
type Env = Map.Map Identifier Type

-- 型制約
-- 等式 S = T の集合。ただし S と T は型。
type Constraint = Set.Set (Type, Type)

-- 型変数の集合
type Variables = Set.Set Type.Variable

newVariable :: TypeChecker Type.Variable
newVariable = do
    state <- State.get
    let nextId = stateNextId state
    let var = Identifier.fromText $ T.pack $ 't' : show nextId
    State.put $ state { stateNextId = nextId + 1 }
    return var

mustBe :: Type -> Env -> Term -> TypeChecker (Constraint, Variables)
mustBe type_ env term = do
    (actualType, constraint, vars) <- typeOf env term
    return (Set.insert (actualType, type_) constraint, vars)

mustBeInt :: Env -> Term -> TypeChecker (Constraint, Variables)
mustBeInt = mustBe Type.Int

mustBeBool :: Env -> Term -> TypeChecker (Constraint, Variables)
mustBeBool = mustBe Type.Bool

typeOfBinOp :: Env -> Operator -> Term -> Term -> TypeChecker (Type, Constraint, Variables)
typeOfBinOp env operator lhs rhs =
    case operator of
        Term.Add -> do
            (lhsConstraint, lhsVars) <- mustBeInt env lhs
            (rhsConstraint, rhsVars) <- mustBeInt env rhs
            return (Type.Int, lhsConstraint <> rhsConstraint, lhsVars <> rhsVars)

        Term.Sub -> do
            (lhsConstraint, lhsVars) <- mustBeInt env lhs
            (rhsConstraint, rhsVars) <- mustBeInt env rhs
            return (Type.Int, lhsConstraint <> rhsConstraint, lhsVars <> rhsVars)

        Term.Mul -> do
            (lhsConstraint, lhsVars) <- mustBeInt env lhs
            (rhsConstraint, rhsVars) <- mustBeInt env rhs
            return (Type.Int, lhsConstraint <> rhsConstraint, lhsVars <> rhsVars)

        Term.Div -> do
            (lhsConstraint, lhsVars) <- mustBeInt env lhs
            (rhsConstraint, rhsVars) <- mustBeInt env rhs
            return (Type.Int, lhsConstraint <> rhsConstraint, lhsVars <> rhsVars)

        Term.And -> do
            (lhsConstraint, lhsVars) <- mustBeBool env lhs
            (rhsConstraint, rhsVars) <- mustBeBool env rhs
            return (Type.Bool, lhsConstraint <> rhsConstraint, lhsVars <> rhsVars)

        Term.Or -> do
            (lhsConstraint, lhsVars) <- mustBeBool env lhs
            (rhsConstraint, rhsVars) <- mustBeBool env rhs
            return (Type.Bool, lhsConstraint <> rhsConstraint, lhsVars <> rhsVars)

        Term.Equal -> do
            (lhsType, lhsConstraint, lhsVars) <- typeOf env lhs
            (rhsType, rhsConstraint, rhsVars) <- typeOf env rhs
            return ( Type.Bool
                   , Set.insert (lhsType, rhsType) (lhsConstraint <> rhsConstraint)
                   , lhsVars <> rhsVars
                   )

typeOf :: Env -> Term -> TypeChecker (Type, Constraint, Variables)
typeOf env term =
    case term of
        Term.Bool _ _ ->
            return (Type.Bool, Set.empty, Set.empty)

        Term.Int _ _ ->
            return (Type.Int, Set.empty, Set.empty)

        Term.If pos condTerm thenTerm elseTerm -> do
            (condType, condConstraint, condVars) <- typeOf env condTerm
            (thenType, thenConstraint, thenVars) <- typeOf env thenTerm
            (elseType, elseConstraint, elseVars) <- typeOf env elseTerm
            let constraint =
                    Set.insert (condType, Type.Bool) $
                        Set.insert (thenType, elseType) $
                            condConstraint <> thenConstraint <> elseConstraint
            let variables = condVars <> thenVars <> elseVars
            return (thenType, constraint, variables)

        Term.Variable pos identifier ->
            case Map.lookup identifier env of
                Just type_ ->
                    return (type_, Set.empty, Set.empty)
                Nothing ->
                    Except.throwError $ TypeError $
                        T.pack (Term.sourcePosPretty pos)
                        <> ": variable `"
                        <> Identifier.name identifier
                        <> "` not found"

        Term.Lambda pos argName body -> do
            var <- newVariable
            let argType = Type.Var var
            let bodyEnv = Map.insert argName argType env
            (bodyType, bodyConstraint, bodyVars) <- typeOf bodyEnv body
            return ( Type.Function argType bodyType
                   , bodyConstraint
                   , Set.insert var bodyVars
                   )

        Term.Apply pos fun arg -> do
            (funType, funConstraint, funVars) <- typeOf env fun
            (argType, argConstraint, argVars) <- typeOf env arg
            var <- newVariable
            let retType = Type.Var var
            let constraint =
                    Set.insert (funType, Type.Function argType retType) $
                        funConstraint <> argConstraint
            let variables = Set.insert var (funVars <> argVars)
            return (retType, constraint, variables)

        Term.BinOp pos operator lhs rhs ->
            typeOfBinOp env operator lhs rhs

        Term.Let pos name expr body -> do
            (exprType, exprConstraint, exprVars) <- typeOf env expr
            let bodyEnv = Map.insert name exprType env
            (bodyType, bodyConstraint, bodyVars) <- typeOf bodyEnv body
            return (bodyType, exprConstraint <> bodyConstraint, exprVars <> bodyVars)

typeCheck :: Term -> Either TypeError Type
typeCheck term =
    undefined
    {-
    State.evalState
        (Except.runExceptT (typeOf Map.empty term))
        TypingState { stateNextId = 1 }
    -}
