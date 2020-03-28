{-# LANGUAGE OverloadedStrings #-}
module Parse(parse) where

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
    (\arg1 maybeArg2 -> case maybeArg2 of
        Just arg2 -> Type.Function arg1 arg2
        Nothing   -> arg1
    )
    <$> boolType
    <*> Parsec.optional
        (  symbol "->"
        *> type_
        )

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

term_ :: Parser Term
term_ =
    Parsec.choice
        [ boolLiteral
        , lambdaExpr
        , variable
        ]
    <?> "term"

term :: Parser Term
term =
    (\term1 maybeTerm2 -> case maybeTerm2 of
        Just term2 -> Term.Apply term1 term2
        Nothing    -> term1
    )
    <$> term_
    <*> Parsec.optional term

--------------------------------------------------------------------------------

parse :: Text -> Either String Term
parse source =
    case Parsec.parse term "" source of
        Left errors  -> Left (Parsec.errorBundlePretty errors)
        Right result -> Right result
