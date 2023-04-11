{-# LANGUAGE FlexibleContexts #-}

module ExpressionParser (parseExpressionsFromFile) where

import Text.Parsec
    ( anyChar,
      newline,
      string,
      eof,
      manyTill,
      (<?>),
      (<|>),
      many,
      try,
      ParseError )
import Text.Parsec.String (Parser, parseFromFile)
import Text.Parsec.Language (haskellDef)
import qualified Text.Parsec.Token as P
import Text.Parsec.Expr (buildExpressionParser, Operator, Assoc(..), Operator(Infix), OperatorTable)
import Control.Monad (void)
import System.IO (FilePath)
import Data.Functor.Identity (Identity)
import Data.Maybe (catMaybes)


data Expression
  = IntegerLiteral Integer
  | Variable String
  | BinOp String Expression Expression
  deriving (Show)

lexer = P.makeTokenParser haskellDef

integer :: Parser Integer
integer = P.integer lexer

parens :: Parser a -> Parser a
parens = P.parens lexer

identifier :: Parser String
identifier = P.identifier lexer

reservedOp :: String -> Parser ()
reservedOp = P.reservedOp lexer

whiteSpace :: Parser ()
whiteSpace = P.whiteSpace lexer

expression :: Parser Expression
expression = buildExpressionParser table term <?> "expression"

table :: OperatorTable String () Identity Expression
table =
  [ [binary "*" (BinOp "*"), binary "/" (BinOp "/")]
  , [binary "+" (BinOp "+"), binary "-" (BinOp "-")]
  ]
  where
    binary name fun = Infix (reservedOp name >> return fun) AssocLeft

term :: Parser Expression
term = parens expression
   <|> fmap IntegerLiteral integer
   <|> fmap Variable identifier
   <?> "term"

-- parseLine :: Parser Expression
-- parseLine = do
--   void $ manyTill anyChar (try (string "="))
--   whiteSpace
--   e <- expression
--   whiteSpace
--   void (many newline)
--   return e

parseLine :: Parser (Maybe Expression)
parseLine = try (Just <$> parseExpression) <|> (skipLine >> return Nothing)
  where
    parseExpression = do
      void $ manyTill anyChar (try (string "="))
      whiteSpace
      e <- expression
      whiteSpace
      void (many newline)
      return e

    skipLine = do
      void $ manyTill anyChar (void newline <|> eof)

parseExpressions :: Parser [Expression]
parseExpressions = do
  whiteSpace
  expressions <- catMaybes <$> many parseLine
  eof
  return expressions

-- parseExpressions :: Parser [Expression]
-- parseExpressions = do
--   whiteSpace
--   expressions <- many parseLine
--   eof
--   return expressions

parseExpressionsFromFile :: FilePath -> IO (Either ParseError [Expression])
parseExpressionsFromFile = parseFromFile parseExpressions
