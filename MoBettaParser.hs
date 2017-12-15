module MoBettaParser where

-- Parser for MoBetta.

import Control.Applicative
import Text.Megaparsec
import Text.Megaparsec.Char -- various basic parsers
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Expr
import Data.Void

import MoBettaAST

type Parser = Parsec Void String


programParser = do
  spaceConsumer
  sepEndBy1 statementParser semicolon <?>  "program"


statementParser = choice
  [   skipStmt
    , printStmt
    , messageStmt
    , readStmt
    , ifStmt
    , whileStmt
    , assignmentStmt
    , blockStmt
  ] where
    skipStmt = lexeme (string "skip") >> return Skip
    printStmt = do
      lexeme (string "print")
      e <- aExpr
      return (Print e)
    readStmt = do
      lexeme (string "read")
      v <- identifier
      return (Read v)
    messageStmt = do
      lexeme (string "message")
      s <- stringLiteral
      lexeme (string "")
      return (Msg s)
    ifStmt = do
      lexeme (string "if")
      c <- bExpr
      lexeme (string "then")
      s1  <- statementParser
      lexeme (string "else")
      s2 <- statementParser
      return (If c s1 s2)
    whileStmt = do
      lexeme (string "while")
      c <- bExpr
      lexeme (string "do")
      s <- statementParser
      return (While c s)
    assignmentStmt = do
      v <- identifier
      a <- assigner
      e <- aExpr
      return (Assign v e)
    blockStmt = do
      lb <- lbrace
      b <- programParser
      rb <- rbrace
      return (Block b)

aExpr = makeExprParser aFactor aOpTable <?> "arithmetic expression"

-- parenthesized expressions are missing
aFactor = choice [ intConst
                , identifierExpr
                , between lparen rparen aExpr
                ] <?> "arithmetic factor"

aOpTable = [ [ prefix  "-"  (AUn Neg)
            , prefix  "+" id ] -- including a prefix + sign
          , [ binary  "*"  (ABin Mul)
            , binary  "/"  (ABin Div)
            , binary  "%"  (ABin Mod)]
          , [ binary  "+"  (ABin Add)
            , binary  "-"  (ABin Sub)  ] ]

bExpr :: Parser BExpr
bExpr = makeExprParser bFactor bOpTable <?> "boolean expression"

bFactor = choice [ boolConst,
                comparison,
                between lparen rparen bExpr
                ] <?> "boolean factor"

bOpTable = [ [ prefix "not"  (BUn Not) ],
             [ binary "and"  (BBin And),
               binary "or"   (BBin Or)] ]

boolConst = choice boolConstTable <?> "boolConst"

boolConstTable = [
      atomic "True" (BoolConst True),
      atomic "False" (BoolConst False)]

-- This is a bit tricky. It is a parser for expressions like x % 2 == 0"
comparison = do
    e1 <- aExpr
    c  <- comparator
    e2 <- aExpr
    return (Reln c e1 e2)

comparator = choice compTable <?> "comparator"

assigner = choice assignTable <?> "assigner"

assignTable = [
    atomic "=" Equal]

compTable = [
    atomic "<"  Less
  , atomic "<=" LessEqual
  , atomic ">"  Greater
  , atomic ">=" GreaterEqual
  , atomic "==" Equal
  , atomic "!=" NEqual
  ]


-- These help declare parsers for operators such as "+", "and", "<=", "not" etc.
binary  opName f = InfixL (atomic opName f) -- make a left associative binary
prefix  opName f = Prefix (atomic opName f) -- make a prefix operator
atomic  opName f = f <$ lexeme (string opName) -- just parse the operator by itself and return a specified result (f).


spaceConsumer :: Parser ()
spaceConsumer = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

-- Define a wrapper that consumes space after a parser
lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

lparen = lexeme (char '(')
rparen = lexeme (char ')')
semicolon = lexeme (char ';')
lbrace = lexeme (char '{')
rbrace = lexeme (char '}')


identifier :: Parser String
identifier = (lexeme . try) p
  where
    p = (:) <$> letterChar <*> many alphaNumChar

identifierExpr = Var <$> identifier

stringLiteral :: Parser String
stringLiteral = char '"' *> manyTill L.charLiteral (char '"')

intConst :: Parser AExpr
intConst = fmap IntConst intConst'
  where
    intConst' = (lexeme . try) ic
    ic = do
          x <- L.decimal -- parse a literal
          notFollowedBy letterChar -- fail if followed by a letter
          return x -- return the  result if we haven't failed

tryit p = parse p "(--)"

mbparse = parse programParser
