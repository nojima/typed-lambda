{-# LANGUAGE OverloadedStrings #-}
module Parse (parse) where

import           Term (Term)
import qualified Term
import           Identifier (Identifier)
import qualified Identifier
import           Type (Type)
import qualified Type
import qualified Data.Char as Char
import           Data.Text (Text)
import qualified Data.Text as T
import           Text.Megaparsec (Parsec, (<?>))
import qualified Text.Megaparsec as Parsec
import qualified Text.Megaparsec.Char as Char
import qualified Text.Megaparsec.Char.Lexer as Lexer
import qualified Text.Megaparsec.Error as Error
import qualified Control.Monad.Combinators.Expr as Expr
import           Data.Void (Void)
import           Data.List (foldl1')
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Set (Set)
import qualified Data.Set as Set

type Parser a = Parsec Void Text a

--------------------------------------------------------------------------------

space :: Parser ()
space =
    Lexer.space
        Char.space1
        (Lexer.skipLineComment "--")
        (Lexer.skipBlockComment "{-" "-}")

lexeme :: Parser a -> Parser a
lexeme =
    Lexer.lexeme space

symbol :: Text -> Parser Text
symbol =
    Lexer.symbol space

decimal :: Parser Integer
decimal =
    lexeme Lexer.decimal

keywords :: Set Text
keywords = Set.fromList
    [ "if"
    , "then"
    , "else"
    , "true"
    , "false"
    , "lambda"
    , "let"
    , "in"
    ]

keyword :: Text -> Parser ()
keyword str =
    lexeme $
        () <$ Char.string str
           <* Parsec.notFollowedBy Char.alphaNumChar

identifierOrKeyword :: Parser Text
identifierOrKeyword =
    let
        alphaChar =
            Parsec.satisfy
                (\c -> Char.isAlpha c && Char.isAscii c)
                <?> "alphabet"

        alphaNumChars =
            Parsec.takeWhileP
                (Just "alphabets or numbers")
                (\c -> (Char.isAlpha c || Char.isNumber c) && Char.isAscii c)
    in
    lexeme (T.cons <$> alphaChar <*> alphaNumChars)

identifier :: Parser Identifier
identifier =
    Parsec.try (do
        offset <- Parsec.getOffset
        word <- identifierOrKeyword
        if Set.member word keywords then
            let
                actual = Error.Tokens (NonEmpty.fromList (T.unpack word))
                expected = Error.Label (NonEmpty.fromList "identifier")
                err = Error.TrivialError offset (Just actual) (Set.singleton expected)
            in
            Parsec.parseError err
        else
            return $ Identifier.Identifier word
    ) <?> "identifier"

--------------------------------------------------------------------------------

boolType :: Parser Type
boolType =
    Type.Bool <$ keyword "Bool"

natType :: Parser Type
natType =
    Type.Int <$ keyword "Int"

atomicType :: Parser Type
atomicType =
    Parsec.choice
        [ boolType
        , natType
        ]

type_ :: Parser Type
type_ =
    let
        types = atomicType `Parsec.sepBy1` symbol "->"
    in
    foldr1 Type.Function <$> types

boolLiteral :: Parser Term
boolLiteral =
    Term.Bool
        <$> Parsec.getSourcePos
        <*> Parsec.choice
            [ True  <$ keyword "true"
            , False <$ keyword "false"
            ]

natLiteral :: Parser Term
natLiteral =
    Term.Int
        <$> Parsec.getSourcePos
        <*> decimal

lambdaExpr :: Parser Term
lambdaExpr =
    Term.Lambda
        <$> Parsec.getSourcePos
        <*  keyword "lambda"
        <*> identifier
        <*  symbol ":"
        <*> type_
        <*  symbol "."
        <*> expr

ifExpr :: Parser Term
ifExpr =
    Term.If
        <$> Parsec.getSourcePos
        <*  keyword "if"
        <*> expr
        <*  keyword "then"
        <*> expr
        <*  keyword "else"
        <*> expr

variable :: Parser Term
variable =
    Term.Variable
        <$> Parsec.getSourcePos
        <*> identifier

parens :: Parser Term
parens =
    Parsec.between
        (symbol "(")
        (symbol ")")
        expr

term_ :: Parser Term
term_ =
    Parsec.choice
        [ boolLiteral
        , natLiteral
        , lambdaExpr
        , ifExpr
        , variable
        , parens
        ]
    <?> "term"

term :: Parser Term
term =
    let
        successiveTerms = Parsec.some term_
    in
    foldl1'
        (\lhs rhs -> Term.Apply (Term.sourcePos rhs) lhs rhs)
        <$> successiveTerms

expr_ :: Parser Term
expr_ =
    Expr.makeExprParser
        term
        [ [ binaryOperator Expr.InfixL "*" Term.Mul
          , binaryOperator Expr.InfixL "/" Term.Div
          ]
        , [ binaryOperator Expr.InfixL "+" Term.Add
          , binaryOperator Expr.InfixL "-" Term.Sub
          ]
        , [ binaryOperator Expr.InfixL "&&" Term.And ]
        , [ binaryOperator Expr.InfixL "||" Term.Or ]
        , [ binaryOperator Expr.InfixN "==" Term.Equal ]
        ]
        <?> "expression"
    where
        binaryOperator infix_ name op =
            infix_ $
                Term.BinOp
                    <$> Parsec.getSourcePos
                    <*> pure op
                    <*  symbol name

letExpr :: Parser Term
letExpr =
    Term.Let
        <$> Parsec.getSourcePos
        <*  keyword "let"
        <*> identifier
        <*  symbol "="
        <*> expr
        <*  keyword "in"
        <*> expr

expr :: Parser Term
expr =
    Parsec.choice
        [ letExpr
        , expr_
        ]

program :: Parser Term
program =
    space *> expr <* Parsec.eof

--------------------------------------------------------------------------------

parse :: Text -> Either String Term
parse source =
    case Parsec.parse program "" source of
        Left errors  -> Left (Parsec.errorBundlePretty errors)
        Right result -> Right result
