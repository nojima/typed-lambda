{-# LANGUAGE OverloadedStrings #-}
module Parse(parse) where

import           Term(Term)
import qualified Term
import           Data.Text(Text)
import qualified Data.Text as T
import           Text.Megaparsec(Parsec, (<?>))
import qualified Text.Megaparsec as Parsec
import qualified Text.Megaparsec.Char as Char
import qualified Text.Megaparsec.Char.Lexer as Lexer
import           Data.Void(Void)

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

trueLiteral :: Parser Term
trueLiteral =
    Term.True <$ keyword "true"

falseLiteral :: Parser Term
falseLiteral =
    Term.False <$ keyword "false"

zeroLiteral :: Parser Term
zeroLiteral =
    Term.Zero <$ keyword "0" 

ifExpr :: Parser Term
ifExpr =
    Term.If
        <$  keyword "if"
        <*> term
        <*  keyword "then"
        <*> term
        <*  keyword "else"
        <*> term

succExpr :: Parser Term
succExpr =
    Term.Succ
        <$  keyword "succ"
        <*> term

predExpr :: Parser Term
predExpr =
    Term.Pred
        <$  keyword "pred"
        <*> term

isZeroExpr :: Parser Term
isZeroExpr =
    Term.IsZero
        <$  keyword "iszero"
        <*> term

term :: Parser Term
term =
    Parsec.choice
        [ trueLiteral
        , falseLiteral
        , zeroLiteral
        , ifExpr
        , succExpr
        , predExpr
        , isZeroExpr
        ]
    <?> "term"

--------------------------------------------------------------------------------

parse :: Text -> Either String Term
parse source =
    case Parsec.parse term "" source of
        Left errors  -> Left (Parsec.errorBundlePretty errors)
        Right result -> Right result
