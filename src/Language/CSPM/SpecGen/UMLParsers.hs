{- |
   Module      :  Language.CSPM.SpecGen.UMLParsers
   Description :  Parsers for Exprs, Guards and Actions
   Copyright   :  Draper Laboratory
-}

module Language.CSPM.SpecGen.UMLParsers where

import Language.CSPM.SpecGen.UMLSyntax
import qualified Data.Set.Monad as S
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language
import Data.Functor.Identity (Identity)

-- Parsers for Expr, Guard, and Action types

opChars :: [Char]
opChars = "-+*/%:=;&|><~"

opNames :: [String]
opNames = ["-", "+", "*", "/", "%", ":=", ";", "&", "|", "=", "~=", "<", ">", "<=", ">=", "~"]

names :: [String]
names = ["if", "then", "else", "true", "false", "tmr", "in", "GlobalTime"]

type OprTable a = [[Operator String () Identity a]]

def :: LanguageDef ()
def = emptyDef { identStart      = letter,
                 identLetter     = alphaNum,
                 opStart         = oneOf opChars,
                 opLetter        = oneOf opChars,
                 reservedOpNames = opNames,
                 reservedNames   = names }

m_parens :: Parser a -> Parser a
m_identifier :: Parser String
m_reservedOp :: String -> Parser ()
m_reserved :: String -> Parser ()
m_integer :: Parser Integer
m_whiteSpace :: Parser ()
TokenParser{ parens     = m_parens,
             identifier = m_identifier,
             reservedOp = m_reservedOp,
             reserved   = m_reserved,
             integer    = m_integer,
             whiteSpace = m_whiteSpace } = makeTokenParser def

exprParser :: Parser Expr
exprParser = m_whiteSpace >> exprParser' where
  exprParser' = buildExpressionParser exprTable exprTerm <?> "expr"

exprTable :: OprTable Expr
exprTable = [ [Prefix (m_reservedOp "-" >> return ENeg )],
              [Infix  (m_reservedOp "+" >> return (EBop EPlus )) AssocLeft],
              [Infix  (m_reservedOp "-" >> return (EBop EMinus)) AssocLeft],
              [Infix  (m_reservedOp "*" >> return (EBop EMult )) AssocLeft],
              [Infix  (m_reservedOp "/" >> return (EBop EDiv  )) AssocLeft],
              [Infix  (m_reservedOp "%" >> return (EBop EMod  )) AssocLeft],
              [Infix  (m_reservedOp "+" >> return (EBop EPlus )) AssocLeft],
              [Infix  (m_reservedOp "-" >> return (EBop EMinus)) AssocLeft] ]

exprTerm :: Parser Expr
exprTerm = m_parens exprParser
           <|> fmap ELit m_integer
           <|> fmap (EVar . Var . Id) m_identifier
           <|> do { m_reserved "GlobalTime";
                    return GlobalTime }
           <|> do { m_reserved "tmr";
                    name <- m_parens m_identifier;
                    return . Tmr $ BasicNode (NId (Id "") name) (Id "") S.empty }
           <|> do { m_reserved "if";
                    g  <- guardParser;
                    m_reserved "then";
                    e1 <- exprParser;
                    m_reserved "else";
                    e2 <- exprParser;
                    return $ EIf g e1 e2 }

guardParser :: Parser Guard
guardParser = m_whiteSpace >> (guardParser' <* eof) where
  guardParser' = buildExpressionParser guardTable guardTerm <?> "guard"

guardTable :: OprTable Guard
guardTable = [ [Prefix (m_reservedOp "~"  >> return GNot)],
               [Infix  (m_reservedOp "&"  >> return (BBop GAnd )) AssocLeft],
               [Infix  (m_reservedOp "|"  >> return (BBop GOr)) AssocLeft] ]

guardTerm :: Parser Guard
guardTerm = m_parens guardParser
            <|> (m_reserved "true"  >> return (GLit True))
            <|> (m_reserved "false" >> return (GLit False))
            <|> do { m_reserved "in";
                     name <- m_parens m_identifier;
                     return . GIn $ BasicNode (NId (Id "") name) (Id "") S.empty }
            <|> iBopParser "="  GEq
            <|> iBopParser "~=" GNEq
            <|> iBopParser "<"  GLt
            <|> iBopParser ">"  GGt
            <|> iBopParser "<=" GLEq
            <|> iBopParser ">=" GGEq

iBopParser :: String -> GIBop -> Parser Guard
iBopParser s op = try $ do { e1 <- exprParser;
                             m_reservedOp s;
                             e2 <- exprParser;
                             return (IBop op e1 e2) }

actionParser :: Parser Action
actionParser = m_whiteSpace >> (actionParser' <* eof) where
  actionParser' = buildExpressionParser actionTable actionTerm <?> "action"

actionTable :: OprTable Action
actionTable = [ [Infix (m_reservedOp ";" >> return Seq) AssocRight] ]

actionTerm :: Parser Action
actionTerm = m_parens actionParser
             <|> (try $ do { v <- m_identifier;
                             m_reservedOp ":=";
                             e <- exprParser;
                             return (Assign (Var (Id v)) e) } )

parseGuard :: String -> Guard
parseGuard s = case parse guardParser "" s of
                 Left err -> error $ "Error while parsing guard: " ++ show err
                 Right g  -> g

parseAction :: String -> Action
parseAction s = case parse actionParser "" s of
                  Left err -> error $ "Error while parsing action: " ++ show err
                  Right a  -> a
