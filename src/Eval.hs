{-# LANGUAGE OverloadedStrings #-}
module Eval (eval, run, RuntimeError(..)) where

import           Term (Term, Pattern, Operator)
import qualified Term
import           Identifier (Identifier)
import qualified Identifier
import           Value (Value, Frame, RuntimeError(..))
import qualified Predefined
import qualified Value
import qualified Data.Text as T
import qualified Data.Vector as Vector
import qualified Data.Map as Map

evalVariable :: Frame -> Identifier -> Either RuntimeError Value
evalVariable frame identifier =
    case Map.lookup identifier frame of
        Just term ->
            Right term

        Nothing ->
            let
                errorMessage =
                    "undefined variable '"
                    <> Identifier.name identifier
                    <> "'"
            in
            Left $ RuntimeError errorMessage

evalIf :: Frame -> Term -> Term -> Term -> Either RuntimeError Value
evalIf frame condTerm thenTerm elseTerm = do
    condValue <- eval frame condTerm
    cond <- case condValue of
        Value.Bool bool ->
            return bool

        value ->
            let
                errorMessage =
                    "non-bool '"
                    <> T.pack (show value)
                    <> "' used as if condition"
            in
            Left $ RuntimeError errorMessage

    if cond then
        eval frame thenTerm
    else
        eval frame elseTerm

evalMatch :: Frame -> Term -> [(Pattern, Term)] -> Either RuntimeError Value
evalMatch frame expr arms = do
    value <- eval frame expr
    loop value arms
  where
    match :: Value -> Pattern -> Maybe [(Identifier, Value)]
    match value pattern_ =
        case (value, pattern_) of
            (Value.Bool v, Term.PBool _ p) ->
                if v == p then Just [] else Nothing

            (Value.Int v, Term.PInt _ p) ->
                if v == p then Just [] else Nothing

            (Value.Tuple v, Term.PTuple _ ps) ->
                if Vector.length v == length ps then
                    concat <$> sequence (Vector.zipWith match v (Vector.fromList ps))
                else
                    Nothing

            (_, Term.PVar _ name) ->
                if name == "_" then
                    Just []
                else
                    Just [(name, value)]

            _ -> Nothing

    loop _ [] = Left $ RuntimeError "non-exhaustive pattern"
    loop value ((pattern_, term):remainder) =
        case match value pattern_ of
            Nothing ->
                loop value remainder

            Just captures ->
                let newFrame = Map.fromList captures `Map.union` frame in
                eval newFrame term

evalApply :: Frame -> Term -> Term -> Either RuntimeError Value
evalApply frame function argument = do
    functionValue <- eval frame function
    argumentValue <- eval frame argument

    case functionValue of
        Value.Closure closureFrame argumentName body ->
            let
                newFrame = Map.insert argumentName argumentValue closureFrame
            in
            eval newFrame body

        Value.NativeFunction _ f ->
            f argumentValue

        value ->
            let
                errorMessage =
                    "cannot apply non-function '"
                    <> T.pack (show value)
                    <> "'"
            in
            Left $ RuntimeError errorMessage

toInt :: Value -> Either RuntimeError Integer
toInt value =
    case value of
        Value.Int n ->
            Right n
        other ->
            let
                errorMessage =
                    "an integer value is expected, but "
                    <> T.pack (show other)
                    <> " is passed"
            in
            Left $ RuntimeError errorMessage

toBool :: Value -> Either RuntimeError Bool
toBool value =
    case value of
        Value.Bool b ->
            Right b
        other ->
            let
                errorMessage =
                    "a boolean value is expected, but "
                    <> T.pack (show other)
                    <>" is passed"
            in
            Left $ RuntimeError errorMessage

isEqual :: Value -> Value -> Bool
isEqual value1 value2 =
    case (value1, value2) of
        (Value.Bool b1, Value.Bool b2) ->
            b1 == b2
        (Value.Int  i1, Value.Int  i2) ->
            i1 == i2
        (Value.List l1, Value.List l2) ->
            length l1 == length l2 && and (zipWith isEqual l1 l2)
        (Value.Tuple t1, Value.Tuple t2) ->
            and (Vector.zipWith isEqual t1 t2)
        _ ->
            False

evalBinOp :: Frame -> Operator -> Term -> Term -> Either RuntimeError Value
evalBinOp frame operator lhs rhs = do
    lhsValue <- eval frame lhs
    rhsValue <- eval frame rhs

    case operator of
        Term.Add   -> Value.Int  <$> ((+)  <$> toInt  lhsValue <*> toInt  rhsValue)
        Term.Sub   -> Value.Int  <$> ((-)  <$> toInt  lhsValue <*> toInt  rhsValue)
        Term.Mul   -> Value.Int  <$> ((*)  <$> toInt  lhsValue <*> toInt  rhsValue)
        Term.Div   -> Value.Int  <$> (div  <$> toInt  lhsValue <*> toInt  rhsValue)
        Term.And   -> Value.Bool <$> ((&&) <$> toBool lhsValue <*> toBool rhsValue)
        Term.Or    -> Value.Bool <$> ((||) <$> toBool lhsValue <*> toBool rhsValue)
        Term.Equal -> return $ Value.Bool (isEqual lhsValue rhsValue)

evalLet :: Frame -> Identifier -> Term -> Term -> Either RuntimeError Value
evalLet frame name expr body = do
    value <- eval frame expr
    let newFrame = Map.insert name value frame
    eval newFrame body

evalDef :: Frame -> Identifier -> Identifier -> Term -> Term -> Either RuntimeError Value
evalDef frame name arg expr body =
    eval newFrame body
  where
    newClosure = Value.Closure newFrame arg expr
    newFrame = Map.insert name newClosure frame

evalList :: Frame -> [Term] -> Either RuntimeError Value
evalList frame elements = do
    values <- traverse (eval frame) elements
    return $ Value.List values

evalTuple :: Frame -> [Term] -> Either RuntimeError Value
evalTuple frame elements = do
    values <- traverse (eval frame) elements
    return $ Value.Tuple (Vector.fromList values)

eval :: Frame -> Term -> Either RuntimeError Value
eval frame term =
    case term of
        Term.Bool _ value ->
            Right $ Value.Bool value

        Term.Int _ value ->
            Right $ Value.Int value

        Term.If _ condTerm thenTerm elseTerm ->
            evalIf frame condTerm thenTerm elseTerm

        Term.Match _ expr arms ->
            evalMatch frame expr arms

        Term.Variable _ identifier ->
            evalVariable frame identifier

        Term.Lambda _ identifier body ->
            Right $ Value.Closure frame identifier body

        Term.Apply _ function argument ->
            evalApply frame function argument

        Term.BinOp _ operator lhs rhs ->
            evalBinOp frame operator lhs rhs

        Term.Let _ name expr body ->
            evalLet frame name expr body

        Term.Def _ name arg expr body ->
            evalDef frame name arg expr body

        Term.List _ elements ->
            evalList frame elements

        Term.Tuple _ elements ->
            evalTuple frame elements

run :: Term -> Either RuntimeError Value
run term =
    let
        initialFrame =
            foldr
                (\(Predefined.Function name _ f) frame ->
                    Map.insert name (Value.NativeFunction name f) frame)
                Map.empty
                Predefined.functions
    in
    eval initialFrame term
