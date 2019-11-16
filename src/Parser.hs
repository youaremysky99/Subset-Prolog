module Parser where

import Text.Parsec
import Lexer
import AST

type Parser = Parsec [(SourcePos, Token)] ()

tokenP :: (Token -> Maybe a) -> Parser a
tokenP test = token show fst (test . snd)

symbol :: String -> Parser ()
symbol c = tokenP (\t -> case t of
  TSym s -> if s == c then Just () else Nothing
  _ -> Nothing)

functorP :: Parser (String, [Term]) -- functor and relation have the same parser
functorP = do
    name <- tokenP (\t -> case t of
                        (TName s) -> Just s
                        _ -> Nothing)
    _ <- symbol "("                        
    terms <- flip sepBy (symbol ",") $ (termP <|> listP)
    _ <- symbol ")"
    return (name, terms)

listP :: Parser Term
listP = do
    _ <- symbol "["
    terms <- flip sepBy (symbol "," <|> symbol "|") $ (termP <|> listP)
    _ <- symbol "]"
    case terms of
      [] -> return (Atom "nil")
      _ -> return (Func "cons" terms)

termP :: Parser Term
termP = do
    name <- tokenP (\t -> case t of
                        (TName s) -> Just (Atom s)
                        (TVar s) -> Just (Var s)
                        _ -> Nothing)
    case name of -- parser consumes name which can be of atom or functor
      (Atom a) -> (fmap (Func a) . between (symbol "(") (symbol ")")
                 . flip sepBy1 (symbol ",") $ termP) <|> return name
      _ -> return name

{- parse a relation or cut in body of clause -}
relP :: Parser Rel
relP = (symbol "!" *> return Cut) 
       <|> relHeadP

{- parse a relation in head of clause -}
relHeadP :: Parser Rel
relHeadP = fmap (uncurry Rel) functorP

ruleP :: Parser Rule
ruleP = do 
    first <- relHeadP
    second <-  ((symbol ".") *> return [[]] ) <|> 
                ((symbol ":-") *> (flip sepBy (symbol ";") . flip sepBy (symbol",") $ relP) <* (symbol ".")) 
    return (Rule first second)

programP :: Parser Program
programP = fmap Program $ many ruleP

parseProgram :: String -> Either ParseError Program
parseProgram source = do
  tokens  <- parse (tokensL <* eof) "" source
  parse (programP <* eof) "" tokens

parseRel :: String -> Either ParseError Rel
parseRel source = do
  tokens  <- parse (tokensL   <* eof) "" source
  parse (relHeadP <* (symbol ".") <* eof) "" tokens