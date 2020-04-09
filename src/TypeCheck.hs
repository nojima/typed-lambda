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

newtype TypeError = TypeError T.Text

data TypingState = TypingState
    { stateNextId :: !Int }
    deriving (Eq, Show)

type TypeChecker a = Except.ExceptT TypeError (State.State TypingState) a

data TypeScheme = ForAll [Type.Variable] Type

-- 型環境
-- 変数名からその型へのマップ。
type Env = Map.Map Identifier TypeScheme

-- 型制約
-- 等式 S = T の集合。ただし S と T は型。
type Constraints = [Constraint]

data Constraint =
    CEqual Type Type SourcePos

-- 型変数の集合
type Variables = [Type.Variable]

newVariable :: TypeChecker Type.Variable
newVariable = do
    state <- State.get
    let nextId = stateNextId state
    let var = Identifier.fromText $ T.pack $ 't' : show nextId
    State.put $ state { stateNextId = nextId + 1 }
    return var

mustBe :: Type -> SourcePos -> Env -> Term -> TypeChecker (Constraints, Variables)
mustBe type_ pos env term = do
    (actualType, constraints, vars) <- constrain env term
    return (CEqual actualType type_ pos : constraints, vars)

mustBeInt :: SourcePos -> Env -> Term -> TypeChecker (Constraints, Variables)
mustBeInt = mustBe Type.Int

mustBeBool :: SourcePos -> Env -> Term -> TypeChecker (Constraints, Variables)
mustBeBool = mustBe Type.Bool

constrainBinOp :: SourcePos -> Env -> Operator -> Term -> Term -> TypeChecker (Type, Constraints, Variables)
constrainBinOp pos env operator lhs rhs =
    case operator of
        Term.Add -> do
            (lhsConstraints, lhsVars) <- mustBeInt pos env lhs
            (rhsConstraints, rhsVars) <- mustBeInt pos env rhs
            return (Type.Int, lhsConstraints <> rhsConstraints, lhsVars <> rhsVars)

        Term.Sub -> do
            (lhsConstraints, lhsVars) <- mustBeInt pos env lhs
            (rhsConstraints, rhsVars) <- mustBeInt pos env rhs
            return (Type.Int, lhsConstraints <> rhsConstraints, lhsVars <> rhsVars)

        Term.Mul -> do
            (lhsConstraints, lhsVars) <- mustBeInt pos env lhs
            (rhsConstraints, rhsVars) <- mustBeInt pos env rhs
            return (Type.Int, lhsConstraints <> rhsConstraints, lhsVars <> rhsVars)

        Term.Div -> do
            (lhsConstraints, lhsVars) <- mustBeInt pos env lhs
            (rhsConstraints, rhsVars) <- mustBeInt pos env rhs
            return (Type.Int, lhsConstraints <> rhsConstraints, lhsVars <> rhsVars)

        Term.And -> do
            (lhsConstraints, lhsVars) <- mustBeBool pos env lhs
            (rhsConstraints, rhsVars) <- mustBeBool pos env rhs
            return (Type.Bool, lhsConstraints <> rhsConstraints, lhsVars <> rhsVars)

        Term.Or -> do
            (lhsConstraints, lhsVars) <- mustBeBool pos env lhs
            (rhsConstraints, rhsVars) <- mustBeBool pos env rhs
            return (Type.Bool, lhsConstraints <> rhsConstraints, lhsVars <> rhsVars)

        Term.Equal -> do
            (lhsType, lhsConstraints, lhsVars) <- constrain env lhs
            (rhsType, rhsConstraints, rhsVars) <- constrain env rhs
            return ( Type.Bool
                   , CEqual lhsType rhsType pos : lhsConstraints <> rhsConstraints
                   , lhsVars <> rhsVars
                   )

-- 型環境 env における term の型、型制約、型変数を求める。
constrain :: Env -> Term -> TypeChecker (Type, Constraints, Variables)
constrain env term =
    case term of
        Term.Bool _ _ ->
            return (Type.Bool, [], [])

        Term.Int _ _ ->
            return (Type.Int, [], [])

        Term.If pos condTerm thenTerm elseTerm -> do
            (condType, condConstraints, condVars) <- constrain env condTerm
            (thenType, thenConstraints, thenVars) <- constrain env thenTerm
            (elseType, elseConstraints, elseVars) <- constrain env elseTerm
            let constraints =
                    CEqual condType Type.Bool pos :
                    CEqual thenType elseType pos :
                    condConstraints <> thenConstraints <> elseConstraints
            let variables = condVars <> thenVars <> elseVars
            return (thenType, constraints, variables)

        Term.Variable pos identifier ->
            case Map.lookup identifier env of
                Just typeScheme -> do
                    type_ <- instantiate typeScheme
                    return (type_, [], [])
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
            (bodyType, bodyConstraints, bodyVars) <- constrain bodyEnv body
            return ( Type.Function argType bodyType
                   , bodyConstraints
                   , var : bodyVars
                   )

        Term.Apply pos fun arg -> do
            (funType, funConstraints, funVars) <- constrain env fun
            (argType, argConstraints, argVars) <- constrain env arg
            var <- newVariable
            let retType = Type.Var var
            let constraints =
                    CEqual funType (Type.Function argType retType) pos :
                    funConstraints <> argConstraints
            let variables = var : funVars <> argVars
            return (retType, constraints, variables)

        Term.BinOp pos operator lhs rhs ->
            constrainBinOp pos env operator lhs rhs

        Term.Let _ name expr body -> do
            (exprType, exprConstraints, exprVars) <- constrain env expr
            sub <- Except.liftEither $ unify exprConstraints
            let principalType = applySubstitution sub exprType
            let typeScheme = generalize env principalType

            let bodyEnv = Map.insert name typeScheme env
            (bodyType, bodyConstraints, bodyVars) <- constrain bodyEnv body
            return (bodyType, exprConstraints <> bodyConstraints, exprVars <> bodyVars)

-- 型スキームを具体化する
instantiate :: TypeScheme -> TypeChecker Type
instantiate (ForAll vars type_) = do
    sub <- forM vars $ \var -> do
        newVar <- newVariable
        return (var, Type.Var newVar)
    return $ applySubstitution sub type_

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

-- 型代入
type Substitution = [(Type.Variable, Type)]

-- 型制約を充足するような最も一般的な型代入を返す。
unify :: Constraints -> Either TypeError Substitution
unify [] = return []
unify (CEqual type1 type2 pos : cs) =
    if type1 == type2 then
        unify cs
    else
        case (type1, type2) of
            (Type.Var var1, _) ->
                (:) <$> pure (var1, type2) <*> unify (substituteConstraints var1 type2 cs)

            (_, Type.Var var2) ->
                (:) <$> pure (var2, type1) <*> unify (substituteConstraints var2 type1 cs)

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

typeCheck :: Term -> Either TypeError Type
typeCheck term = do
    (type_, constraintss, _) <-
        State.evalState
            (Except.runExceptT (constrain Map.empty term))
            TypingState { stateNextId = 0 }

    sub <- unify constraintss

    return $ applySubstitution sub type_
