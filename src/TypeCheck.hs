{-# LANGUAGE OverloadedStrings #-}
module TypeCheck (typeCheck, TypeError(..)) where

import           Term (Term, Operator, Pattern, SourcePos)
import qualified Term
import           Type (Type, TypeScheme(..))
import qualified Type
import           Identifier (Identifier)
import qualified Identifier
import qualified Predefined
import qualified Control.Monad.Except as Except
import qualified Control.Monad.State.Strict as State
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import           Control.Monad

-------------------------------------------------------------------------------

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

        Type.List element ->
            let ForAll elementVars elementType = generalize env element in
            ForAll elementVars (Type.List elementType)

        Type.Tuple elements ->
            let
                (vars, types) =
                    foldr
                        (\(ForAll vars1 type1) (vars2, types2) -> (vars1 <> vars2, type1:types2))
                        ([], [])
                        (map (generalize env) elements)
            in
            ForAll vars (Type.Tuple types)

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
            Type.List element ->
                f element
            Type.Tuple elements ->
                any f elements
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
substituteType var type_ =
    Type.mapVariable $ \v ->
        if v == var
        then type_
        else Type.Var v

applySubstitution :: Substitution -> Type -> Type
applySubstitution sub =
    let subMap = Map.fromList sub in
    Type.mapVariable $ \v ->
        case Map.lookup v subMap of
            Just t ->
                applySubstitution sub t
            Nothing ->
                Type.Var v

-------------------------------------------------------------------------------

-- 型制約
-- 等式 S = T の集合。ただし S と T は型。
type Constraints = [Constraint]

data Constraint =
    CEqual Type Type Description

instance Show Constraint where
    show (CEqual t1 t2 _) = show t1 ++ " == " ++ show t2

data Description =
    Description SourcePos T.Text
    deriving (Show)

unify :: Constraints -> Either TypeError Substitution
unify cs = unify' (reverse cs) -- エラーメッセージをより直観的にするために reverse する

-- 型制約を充足するような最も一般的な型代入を返す。
unify' :: Constraints -> Either TypeError Substitution
unify' [] = return []
unify' (CEqual type1 type2 desc : cs) =
    if type1 == type2 then
        unify' cs
    else
        case (type1, type2) of
            (Type.Var var1, _) ->
                (:) (var1, type2) <$> unify' (substituteConstraints var1 type2 cs)

            (_, Type.Var var2) ->
                (:) (var2, type1) <$> unify' (substituteConstraints var2 type1 cs)

            (Type.Function arg1 ret1, Type.Function arg2 ret2) ->
                unify' (CEqual arg1 arg2 desc : CEqual ret1 ret2 desc : cs)

            (Type.List element1, Type.List element2) ->
                unify' (CEqual element1 element2 desc : cs)

            (Type.Tuple elements1, Type.Tuple elements2) | length elements1 == length elements2 ->
                let
                    newConstraints =
                        zipWith (\e1 e2 -> CEqual e1 e2 desc) elements1 elements2
                in
                unify' (newConstraints <> cs)

            _ ->
                let
                    Description pos message = desc

                    errorMessage =
                        T.pack (Term.sourcePosPretty pos)
                        <> ": "
                        <> message
                        <> "\n"
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

mustBe :: Type -> SourcePos -> Env -> Term -> T.Text -> TypeChecker Constraints
mustBe type_ pos env term errorMessage = do
    (actualType, constraints) <- constrain env term
    let constraint =
            CEqual actualType type_
                (Description pos errorMessage)
    return (constraint:constraints)

mustBeInt :: SourcePos -> Env -> Term -> T.Text -> TypeChecker Constraints
mustBeInt = mustBe Type.Int

mustBeBool :: SourcePos -> Env -> Term -> T.Text -> TypeChecker Constraints
mustBeBool = mustBe Type.Bool

constrainBinOp :: SourcePos -> Env -> Operator -> Term -> Term -> TypeChecker (Type, Constraints)
constrainBinOp pos env operator lhs rhs =
    case operator of
        Term.Add -> do
            lhsConstraints <- mustBeInt pos env lhs "1st argument of `+` must be Int"
            rhsConstraints <- mustBeInt pos env rhs "2nd argument of `+` must be Int"
            return (Type.Int, lhsConstraints <> rhsConstraints)

        Term.Sub -> do
            lhsConstraints <- mustBeInt pos env lhs "1st argument of `-` must be Int"
            rhsConstraints <- mustBeInt pos env rhs "2nd argument of `-` must be Int"
            return (Type.Int, lhsConstraints <> rhsConstraints)

        Term.Mul -> do
            lhsConstraints <- mustBeInt pos env lhs "1st argument of `*` must be Int"
            rhsConstraints <- mustBeInt pos env rhs "2nd argument of `*` must be Int"
            return (Type.Int, lhsConstraints <> rhsConstraints)

        Term.Div -> do
            lhsConstraints <- mustBeInt pos env lhs "1st argument of `/` must be Int"
            rhsConstraints <- mustBeInt pos env rhs "2nd argument of `/` must be Int"
            return (Type.Int, lhsConstraints <> rhsConstraints)

        Term.And -> do
            lhsConstraints <- mustBeBool pos env lhs "1st argument of `&&` must be Bool"
            rhsConstraints <- mustBeBool pos env rhs "2nd argument of `&&` must be Bool"
            return (Type.Bool, lhsConstraints <> rhsConstraints)

        Term.Or -> do
            lhsConstraints <- mustBeBool pos env lhs "1st argument of `||` must be Bool"
            rhsConstraints <- mustBeBool pos env rhs "2nd argument of `||` must be Bool"
            return (Type.Bool, lhsConstraints <> rhsConstraints)

        Term.Equal -> do
            (lhsType, lhsConstraints) <- constrain env lhs
            (rhsType, rhsConstraints) <- constrain env rhs
            let constraint =
                    CEqual lhsType rhsType
                        (Description pos "both sides of `==` must be the same type")
            return (Type.Bool, constraint : lhsConstraints <> rhsConstraints)

constrainMatch :: SourcePos -> Env -> Term -> [(Pattern, Term)] -> TypeChecker (Type, Constraints)
constrainMatch pos env expr arms = do
    (exprType, exprConstraints) <- constrain env expr
    undefined
  where
    match :: Pattern -> (Type, [(Identifier, Type)])

calculatePrincipalType :: Env -> Type -> Constraints -> TypeChecker TypeScheme
calculatePrincipalType env type_ constraints = do
    sub <- Except.liftEither $ unify constraints
    let principalType = applySubstitution sub type_
    return $ generalize env principalType

-- 型環境 env における term の型と型制約を求める。
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
            let condConstraint =
                    CEqual condType Type.Bool
                        (Description pos "condition of `if` expression must be Bool")
            let thenElseConstraint =
                    CEqual elseType thenType
                        (Description pos "`then` and `else` have incompatible types")
            let constraints =
                    condConstraint : thenElseConstraint :
                    condConstraints <> thenConstraints <> elseConstraints
            return (thenType, constraints)

        Term.Match pos expr arms ->
            constrainMatch pos env expr arms

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
            let constraint =
                    CEqual (Type.Function argType retType) funType
                        (Description pos "type mismatch in the function call")
            let constraints =
                    constraint : funConstraints <> argConstraints
            return (retType, constraints)

        Term.BinOp pos operator lhs rhs ->
            constrainBinOp pos env operator lhs rhs

        Term.Let _ name expr body -> do
            -- expr の主要型を求める
            (exprType, exprConstraints) <- constrain env expr
            exprPrincipalType <- calculatePrincipalType env exprType exprConstraints

            -- body の型と型制約を求める
            let bodyEnv = Map.insert name exprPrincipalType env
            (bodyType, bodyConstraints) <- constrain bodyEnv body
            return (bodyType, bodyConstraints)

        Term.Def pos name argName expr body -> do
            argVar <- newVariable
            let argType = Type.Var argVar

            retVar <- newVariable
            let funType = Type.Function argType (Type.Var retVar)

            -- expr の型と型制約を求める
            let exprEnv =
                    Map.insert argName (ForAll [] argType) $
                        Map.insert name (ForAll [] funType) env
            (exprType, exprConstraints) <- constrain exprEnv expr

            -- expr を本体とする関数の主要型を求める
            let retConstraint =
                    CEqual (Type.Var retVar) exprType
                        (Description pos "type mismatch in the returned value of the function")
            funPrincipalType <- calculatePrincipalType env funType (retConstraint:exprConstraints)

            -- body の型と型制約を求める
            let bodyEnv = Map.insert name funPrincipalType env
            (bodyType, bodyConstraints) <- constrain bodyEnv body
            return (bodyType, bodyConstraints)

        Term.List pos elements ->
            if null elements then do
                elementVar <- newVariable
                let elementType = Type.Var elementVar
                return (Type.List elementType, [])
            else do
                typesAndConstraints <- traverse (constrain env) elements
                let (type_:types, constraints) = unzip typesAndConstraints
                return ( Type.List type_
                       , map (\t -> CEqual t type_ (Description pos "type mismatch")) types
                         <> concat constraints
                       )

        Term.Tuple _ elements ->
            if null elements then
                return (Type.Tuple [], [])
            else do
                typesAndConstraints <- traverse (constrain env) elements
                let (types, constraints) = unzip typesAndConstraints
                return ( Type.Tuple types
                       , concat constraints
                       )

-------------------------------------------------------------------------------

typeCheck :: Term -> Either TypeError Type
typeCheck term = do
    let initialEnv =
            Map.fromList $
                map (\(Predefined.Function name typeScheme _) -> (name, typeScheme))
                Predefined.functions

    (type_, constraintss) <-
        State.evalState
            (Except.runExceptT (constrain initialEnv term))
            TypingState { stateNextId = 0 }

    sub <- unify constraintss

    return $ applySubstitution sub type_
