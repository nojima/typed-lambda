{-# LANGUAGE OverloadedStrings #-}
module TypeCheck (typeCheck, TypeError(..)) where

import           Term (Term, Operator, SourcePos)
import qualified Term
import           Type (Type)
import qualified Type
import           Identifier (Identifier)
import qualified Identifier
import qualified Control.Monad.Except as Except
import qualified Control.Monad.State.Strict as State
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import           Control.Monad

-------------------------------------------------------------------------------

data TypeScheme = ForAll [Type.Variable] Type

-- 型を一般化して型スキームにする。
generalize :: Env -> Type -> TypeScheme
generalize env type_ =
    case type_ of
        Type.Function arg ret ->
            let
                ForAll argVars argType = generalize env arg
                ForAll retVars retType = generalize env ret
            in
            ForAll (argVars <> retVars) (Type.Function argType retType)

        Type.Var var | not (variableExists var env) ->
            ForAll [var] type_

        _ ->
            ForAll [] type_

-- 型スキームを具体化する。
instantiate :: TypeScheme -> TypeChecker Type
instantiate (ForAll vars type_) = do
    sub <- forM vars $ \var -> do
        newVar <- newVariable
        return (var, Type.Var newVar)
    return $ applySubstitution sub type_

newVariable :: TypeChecker Type.Variable
newVariable = do
    state <- State.get
    let nextId = stateNextId state
    let var = Identifier.fromText $ T.pack $ 't' : show nextId
    State.put $ state { stateNextId = nextId + 1 }
    return var

-------------------------------------------------------------------------------

-- 型環境
-- 変数名からその型へのマップ。
type Env = Map.Map Identifier TypeScheme

-- 型環境で指定された型変数が言及されているなら True を返す。
variableExists :: Type.Variable -> Env -> Bool
variableExists var =
    any $ \(ForAll _ type_) -> f type_
  where
    f type_ =
        case type_ of
            Type.Function arg ret ->
                f arg || f ret
            Type.Var v | v == var ->
                True
            _ ->
                False

-------------------------------------------------------------------------------

-- 型代入
type Substitution = [(Type.Variable, Type)]

substituteConstraints :: Type.Variable -> Type -> Constraints -> Constraints
substituteConstraints var type_ =
    map $ \(CEqual lhs rhs pos) ->
        CEqual (substituteType var type_ lhs) (substituteType var type_ rhs) pos

substituteType :: Type.Variable -> Type -> Type -> Type
substituteType var type_ target =
    case target of
        Type.Function arg ret ->
            Type.Function (substituteType var type_ arg) (substituteType var type_ ret)
        Type.Var v | v == var ->
            type_
        _ ->
            target

applySubstitution :: Substitution -> Type -> Type
applySubstitution sub =
    apply (Map.fromList sub)
  where
    apply :: Map.Map Type.Variable Type -> Type -> Type
    apply sub_ type_ =
        case type_ of
            Type.Function arg ret ->
                Type.Function (apply sub_ arg) (apply sub_ ret)
            Type.Var v ->
                case Map.lookup v sub_ of
                    Just t ->
                        apply sub_ t
                    Nothing ->
                        type_
            _ ->
                type_

-------------------------------------------------------------------------------

-- 型制約
-- 等式 S = T の集合。ただし S と T は型。
type Constraints = [Constraint]

data Constraint =
    CEqual Type Type SourcePos

-- 型制約を充足するような最も一般的な型代入を返す。
unify :: Constraints -> Either TypeError Substitution
unify [] = return []
unify (CEqual type1 type2 pos : cs) =
    if type1 == type2 then
        unify cs
    else
        case (type1, type2) of
            (Type.Var var1, _) ->
                (:) (var1, type2) <$> unify (substituteConstraints var1 type2 cs)

            (_, Type.Var var2) ->
                (:) (var2, type1) <$> unify (substituteConstraints var2 type1 cs)

            (Type.Function arg1 ret1, Type.Function arg2 ret2) ->
                unify (CEqual arg1 arg2 pos : CEqual ret1 ret2 pos : cs)

            _ ->
                let
                    errorMessage =
                        T.pack (Term.sourcePosPretty pos)
                        <> ": type mismatch\n"
                        <> "  expected: "
                        <> Type.pretty type2
                        <> "\n"
                        <> "  actual: "
                        <> Type.pretty type1
                        <> "\n"
                in
                Left $ TypeError errorMessage

-------------------------------------------------------------------------------

type TypeChecker a = Except.ExceptT TypeError (State.State TypingState) a

newtype TypeError = TypeError T.Text

newtype TypingState = TypingState
    { stateNextId :: Int }
    deriving (Eq, Show)

mustBe :: Type -> SourcePos -> Env -> Term -> TypeChecker Constraints
mustBe type_ pos env term = do
    (actualType, constraints) <- constrain env term
    return $ CEqual actualType type_ pos : constraints

mustBeInt :: SourcePos -> Env -> Term -> TypeChecker Constraints
mustBeInt = mustBe Type.Int

mustBeBool :: SourcePos -> Env -> Term -> TypeChecker Constraints
mustBeBool = mustBe Type.Bool

constrainBinOp :: SourcePos -> Env -> Operator -> Term -> Term -> TypeChecker (Type, Constraints)
constrainBinOp pos env operator lhs rhs =
    case operator of
        Term.Add -> do
            lhsConstraints <- mustBeInt pos env lhs
            rhsConstraints <- mustBeInt pos env rhs
            return (Type.Int, lhsConstraints <> rhsConstraints)

        Term.Sub -> do
            lhsConstraints <- mustBeInt pos env lhs
            rhsConstraints <- mustBeInt pos env rhs
            return (Type.Int, lhsConstraints <> rhsConstraints)

        Term.Mul -> do
            lhsConstraints <- mustBeInt pos env lhs
            rhsConstraints <- mustBeInt pos env rhs
            return (Type.Int, lhsConstraints <> rhsConstraints)

        Term.Div -> do
            lhsConstraints <- mustBeInt pos env lhs
            rhsConstraints <- mustBeInt pos env rhs
            return (Type.Int, lhsConstraints <> rhsConstraints)

        Term.And -> do
            lhsConstraints <- mustBeBool pos env lhs
            rhsConstraints <- mustBeBool pos env rhs
            return (Type.Bool, lhsConstraints <> rhsConstraints)

        Term.Or -> do
            lhsConstraints <- mustBeBool pos env lhs
            rhsConstraints <- mustBeBool pos env rhs
            return (Type.Bool, lhsConstraints <> rhsConstraints)

        Term.Equal -> do
            (lhsType, lhsConstraints) <- constrain env lhs
            (rhsType, rhsConstraints) <- constrain env rhs
            return ( Type.Bool
                   , CEqual lhsType rhsType pos : lhsConstraints <> rhsConstraints
                   )

-- 型環境 env における term の型、型制約、型変数を求める。
constrain :: Env -> Term -> TypeChecker (Type, Constraints)
constrain env term =
    case term of
        Term.Bool _ _ ->
            return (Type.Bool, [])

        Term.Int _ _ ->
            return (Type.Int, [])

        Term.If pos condTerm thenTerm elseTerm -> do
            (condType, condConstraints) <- constrain env condTerm
            (thenType, thenConstraints) <- constrain env thenTerm
            (elseType, elseConstraints) <- constrain env elseTerm
            let constraints =
                    CEqual condType Type.Bool pos :
                    CEqual thenType elseType pos :
                    condConstraints <> thenConstraints <> elseConstraints
            return (thenType, constraints)

        Term.Variable pos identifier ->
            case Map.lookup identifier env of
                Just typeScheme -> do
                    type_ <- instantiate typeScheme
                    return (type_, [])
                Nothing ->
                    Except.throwError $ TypeError $
                        T.pack (Term.sourcePosPretty pos)
                        <> ": variable `"
                        <> Identifier.name identifier
                        <> "` not found"

        Term.Lambda _ argName body -> do
            var <- newVariable
            let argType = Type.Var var
            let bodyEnv = Map.insert argName (ForAll [] argType) env
            (bodyType, bodyConstraints) <- constrain bodyEnv body
            return ( Type.Function argType bodyType
                   , bodyConstraints
                   )

        Term.Apply pos fun arg -> do
            (funType, funConstraints) <- constrain env fun
            (argType, argConstraints) <- constrain env arg
            var <- newVariable
            let retType = Type.Var var
            let constraints =
                    CEqual funType (Type.Function argType retType) pos :
                    funConstraints <> argConstraints
            return (retType, constraints)

        Term.BinOp pos operator lhs rhs ->
            constrainBinOp pos env operator lhs rhs

        Term.Let _ name expr body -> do
            (exprType, exprConstraints) <- constrain env expr
            sub <- Except.liftEither $ unify exprConstraints
            let principalType = applySubstitution sub exprType
            let typeScheme = generalize env principalType

            let bodyEnv = Map.insert name typeScheme env
            (bodyType, bodyConstraints) <- constrain bodyEnv body
            return (bodyType, exprConstraints <> bodyConstraints)

        Term.Def _ name argName expr body -> do
            argVar <- newVariable
            let argType = Type.Var argVar

            retVar <- newVariable
            let funType = Type.Function argType (Type.Var retVar)

            let exprEnv =
                    Map.insert argName (ForAll [] argType) $
                        Map.insert name (ForAll [] funType) env

            (_, exprConstraints) <- constrain exprEnv expr
            sub <- Except.liftEither $ unify exprConstraints
            let principalType = applySubstitution sub funType
            let typeScheme = generalize env principalType

            let bodyEnv = Map.insert name typeScheme env
            (bodyType, bodyConstraints) <- constrain bodyEnv body
            return (bodyType, exprConstraints <> bodyConstraints)

-------------------------------------------------------------------------------

typeCheck :: Term -> Either TypeError Type
typeCheck term = do
    (type_, constraintss) <-
        State.evalState
            (Except.runExceptT (constrain Map.empty term))
            TypingState { stateNextId = 0 }

    sub <- unify constraintss

    return $ applySubstitution sub type_
