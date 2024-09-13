module Parser (parseLambda, parseLine) where

import Control.Monad
import Control.Applicative
import Data.Char
import Lambda
import Binding

newtype Parser a = Parser { parse :: String -> Maybe (a, String) }

instance Functor Parser where
  fmap f (Parser p) = Parser $ \s -> do
    (result, rest) <- p s
    return (f result, rest)

instance Applicative Parser where
  pure x = Parser $ \s -> Just (x, s)
  (Parser pf) <*> (Parser p) = Parser $ \s -> do
    (f, s1) <- pf s
    (result, s2) <- p s1
    return (f result, s2)

instance Monad Parser where
  (Parser p) >>= f = Parser $ \s -> do
    (result, s1) <- p s
    parse (f result) s1

instance Alternative Parser where
  empty = failParser
  (Parser p1) <|> (Parser p2) = Parser $ \s -> p1 s <|> p2 s

  -- Parse a failure
failParser :: Parser a
failParser = Parser $ const Nothing

-- Parse a single character
charParser :: Char -> Parser Char
charParser c = Parser $ \s -> case s of
  (x:xs) | x == c -> Just (c, xs)
  _ -> Nothing

-- Parse a character satisfying a predicate
predicateParser :: (Char -> Bool) -> Parser Char
predicateParser p = Parser $ \s -> case s of
  (x:xs) | p x -> Just (x, xs)
  _ -> Nothing

-- Parse a specific string
stringParser :: String -> Parser String
stringParser = mapM charParser

-- Parse a variable
varParser :: Parser String
varParser = (:) <$> predicateParser isLower <*> many (predicateParser isLower)

-- Parse a macro
macroParser :: Parser String
macroParser = some (predicateParser isUpper <|> predicateParser isDigit)

-- Parse whitespace
whitespaceParser :: Parser String
whitespaceParser = many (charParser ' ')

-- Parse a variable lambda expression
varLambda :: Parser Lambda
varLambda = Var <$> varParser

-- Parse a macro lambda expression
macroLambda :: Parser Lambda
macroLambda = Macro <$> macroParser

-- Parse an abstraction lambda expression
absLambda :: Parser Lambda
absLambda = do
  charParser '\\'
  var <- varParser
  charParser '.'
  body <- lambdaParser
  return $ Abs var body

-- Parse an application lambda expression
appLambda :: Parser Lambda
appLambda = do
  charParser '('
  e1 <- lambdaParser
  whitespaceParser
  e2 <- lambdaParser
  charParser ')'
  return $ App e1 e2

-- Combine all lambda parsers
lambdaParser :: Parser Lambda
lambdaParser = varLambda <|> macroLambda <|> absLambda <|> appLambda

-- 2.1. / 3.2.
-- Main function to parse a lambda expression from a string
parseLambda :: String -> Lambda
parseLambda input = case parse lambdaParser input of
  Just (result, "") -> result
  _ -> error "Invalid lambda expression"

-- 3.3.
-- Parse a line of code
parseLine :: String -> Either String Line
parseLine input = case parse lineParser input of
  Just (result, "") -> Right result
  _ -> Left "Invalid line of code"

-- Parse a binding or an evaluation
lineParser :: Parser Line
lineParser = bindingParser <|> evalParser

-- Parse a binding line
bindingParser :: Parser Line
bindingParser = do
  name <- macroParser
  whitespaceParser
  charParser '='
  whitespaceParser
  expr <- lambdaParser
  return $ Binding name expr

-- Parse an evaluation line
evalParser :: Parser Line
evalParser = Eval <$> lambdaParser
