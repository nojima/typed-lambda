{-# LANGUAGE OverloadedStrings #-}
module Parse (parse) where

import           Term (Term)
import qualified Term
import           Identifier (Identifier(..))
import           Type (Type)
import qualified Type
import qualified Data.Char as Char
import           Data.Text (Text)
import qualified Data.Text as T
import           Text.Megaparsec (Parsec, (<?>))
import qualified Text.Megaparsec as Parsec
import qualified Text.Megaparsec.Char as Char
import qualified Text.Megaparsec.Char.Lexer as Lexer
import           Data.Void (Void)
import           Data.List (foldl1')

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

keyword :: Text -> Parser ()
keyword str =
    lexeme $
        () <$ Char.string str
           <* Parsec.notFollowedBy Char.alphaNumChar

--------------------------------------------------------------------------------

identifier :: Parser Identifier
identifier =
    let
        alphaChar =
            Parsec.satisfy
                (\c -> Char.isAlpha c && Char.isAscii c)
                <?> "alphabet"

        alphaNumChars =
            Parsec.takeWhileP
                (Just "alphabets or numbers")
                (\c -> (Char.isAlpha c || Char.isNumber c) && Char.isAscii c)

        parser =
            lexeme (T.cons <$> alphaChar <*> alphaNumChars)
    in
    Identifier <$> parser <?> "identifier"

boolType :: Parser Type
boolType =
    Type.Bool <$ keyword "Bool"

type_ :: Parser Type
type_ =
    let
        types = boolType `Parsec.sepBy` symbol "->"
    in
    foldr1 Type.Function <$> types

boolLiteral :: Parser Term
boolLiteral =
    Parsec.choice
        [ Term.Bool True  <$ keyword "true"
        , Term.Bool False <$ keyword "false"
        ]

lambdaExpr :: Parser Term
lambdaExpr =
    Term.Lambda
        <$  keyword "lambda"
        <*> identifier
        <*  symbol ":"
        <*> type_
        <*  symbol "."
        <*> term

variable :: Parser Term
variable =
    Term.Variable <$> identifier

parens :: Parser Term
parens =
    Parsec.between
        (symbol "(")
        (symbol ")")
        term

term_ :: Parser Term
term_ =
    Parsec.choice
        [ boolLiteral
        , lambdaExpr
        , variable
        , parens
        ]
    <?> "term"

term :: Parser Term
term =
    let
        successiveTerms = Parsec.some term_
    in
    foldl1' Term.Apply <$> successiveTerms

program :: Parser Term
program =
    space *> term <* Parsec.eof

--------------------------------------------------------------------------------

parse :: Text -> Either String Term
parse source =
    case Parsec.parse program "" source of
        Left errors  -> Left (Parsec.errorBundlePretty errors)
        Right result -> Right result
