{-# LANGUAGE OverloadedStrings #-}
module Parse (parse) where

import           Term (Term, Pattern)
import qualified Term
import           Identifier (Identifier)
import qualified Identifier
import qualified Control.Monad.Combinators.Expr as Expr
import qualified Data.Char as Char
import           Data.Function ((&))
import           Data.List (foldl')
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Void (Void)
import           Text.Megaparsec (Parsec, (<?>))
import qualified Text.Megaparsec as Parsec
import qualified Text.Megaparsec.Char as Char
import qualified Text.Megaparsec.Char.Lexer as Lexer
import qualified Text.Megaparsec.Error as Error

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

symbol :: Text -> Parser ()
symbol s =
    () <$ Lexer.symbol space s

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
    , "def"
    , "match"
    , "with"
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
                (\c -> (Char.isAlpha c || c == '_') && Char.isAscii c)
                <?> "alphabet"

        alphaNumChars =
            Parsec.takeWhileP
                (Just "alphabets or numbers")
                (\c -> (Char.isAlpha c || Char.isNumber c || c == '_') && Char.isAscii c)
    in
    lexeme (T.cons <$> alphaChar <*> alphaNumChars)

identifier :: Parser Identifier
identifier =
    Parsec.try $ Parsec.label "identifier" $ do
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
            return $ Identifier.fromText word

--------------------------------------------------------------------------------

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

listLiteral :: Parser Term
listLiteral =
    Term.List
        <$> Parsec.getSourcePos
        <*  symbol "["
        <*> expr `Parsec.sepBy` symbol ","
        <*  symbol "]"

lambdaExpr :: Parser Term
lambdaExpr =
    Term.Lambda
        <$> Parsec.getSourcePos
        <*  keyword "lambda"
        <*> identifier
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
parens = do
    pos <- Parsec.getSourcePos
    symbol "("
    Parsec.choice
        [ do
            -- Pattern 1: empty tuple
            symbol ")"
            return $ Term.Tuple pos []
        , do
            e <- expr
            Parsec.choice
                [ do
                    -- Pattern 2: expression enclosed by parentheses
                    symbol ")"
                    return e
                , do
                    -- Pattern 3: nonempty tuple
                    symbol ","
                    es <- expr `Parsec.sepBy` symbol ","
                    symbol ")"
                    return $ Term.Tuple pos (e:es)
                ]
        ]

simpleTerm :: Parser Term
simpleTerm =
    Parsec.choice
        [ boolLiteral
        , natLiteral
        , listLiteral
        , lambdaExpr
        , ifExpr
        , variable
        , parens
        ]
    <?> "term"

term :: Parser Term
term = do
    t <- simpleTerm
    args <- Parsec.many argument
    return $ foldl' (&) t args
  where
    argument = do
        pos <- Parsec.getSourcePos
        arg <- simpleTerm
        return (\fun -> Term.Apply pos fun arg)

simpleExpr :: Parser Term
simpleExpr =
    Expr.makeExprParser
        term
        [ [ binaryOperator Expr.InfixL "*" Term.Mul
          , binaryOperator Expr.InfixL "/" Term.Div
          ]
        , [ binaryOperator Expr.InfixL "+" Term.Add
          , binaryOperator Expr.InfixL "-" Term.Sub
          ]
        , [ binaryOperator Expr.InfixN "==" Term.Equal ]
        , [ binaryOperator Expr.InfixL "&&" Term.And ]
        , [ binaryOperator Expr.InfixL "||" Term.Or ]
        ]
        <?> "expression"
  where
    binaryOperator infix_ name op =
        infix_ $ Parsec.label "binary operator" $
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

defExpr :: Parser Term
defExpr =
    Term.Def
        <$> Parsec.getSourcePos
        <*  keyword "def"
        <*> identifier
        <*> identifier
        <*  symbol "="
        <*> expr
        <*  keyword "in"
        <*> expr

pattern :: Parser Pattern
pattern = do
    pos <- Parsec.getSourcePos
    Parsec.choice
        [ Term.PBool  pos <$> (keyword "true"  *> pure True)
        , Term.PBool  pos <$> (keyword "false" *> pure False)
        , Term.PInt   pos <$> decimal
        , Term.PVar   pos <$> identifier
        , Term.PTuple pos <$> tuple
        ]
  where
    tuple =
        symbol "("
        *> pattern `Parsec.sepBy` symbol ","
        <* symbol ")"

matchExpr :: Parser Term
matchExpr =
    Term.Match
        <$> Parsec.getSourcePos
        <*  keyword "match"
        <*> expr
        <*  keyword "with"
        <*> Parsec.some arm
  where
    arm =
        (,)
            <$  symbol "|"
            <*> pattern
            <*  symbol "->"
            <*> expr

expr :: Parser Term
expr =
    Parsec.choice
        [ letExpr
        , defExpr
        , matchExpr
        , simpleExpr
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
