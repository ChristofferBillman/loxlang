module Tokens where 

-- Datatype TokenType
-- Used for assigning types of tokens
data TokenType = LEFT_PAREN | RIGHT_PAREN | LEFT_BRACE | RIGHT_BRACE |COMMA | DOT | MINUS | PLUS | SEMICOLON | SLASH | STAR | BANG | BANG_EQUAL | EQUAL | EQUAL_EQUAL | GREATER | GREATER_EQUAL | LESS | LESS_EQUAL | IDENTIFIER | STRING | NUMBER | AND | CLASS | ELSE | FALSE | FUN | FOR | IF | NIL | OR | PRINT | RETURN | SUPER | THIS | TRUE | VAR | WHILE | EOF deriving (Show, Eq)


-- Datatype Literal
-- Used for storing the actual values of string literals, identifiers, and numbers
data Literal = NONE | STR String | ID String | NUM Float | FALSE_LIT | TRUE_LIT | NIL_LIT deriving (Show)

-- Datatype Token
-- Represents the tokens the scanner produces and which are used as input for the parser
data Token = TOKEN TokenType String Literal Int deriving (Show)
