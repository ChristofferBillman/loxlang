{-
    LOX Scanner/Lexer
    Version: 2022-03-03
    Author: Christoffer Billman

    Changes version 2022-03-03:
    * Line numbering starts on 1 instead of 0.
    * String is included in tokens of 2-character lexemes (line 75).
    * Identifiers FLASE, TRUE and NIL are assigned literal
      FALSE_LIT, TRUE_LIT and NIL_LIT, respectivley, instead of NONE.
-}
module Scanner (scanTokens, loxError) where

import Tokens
import Data.Char (isAlphaNum, isDigit, isAlpha)

-- Extracts tokens from a given string of lox code.
scanTokens :: String -> [Token]
scanTokens x = scanTokensPrivate x 1

-- Scans a given list of characters for lox tokens.
-- The purpose of extractin g this method from scanTokens is to hide
-- the extra "line" parameter from the user.
-- NOTE: A call to addToken means that the recursion will continue.
scanTokensPrivate :: [Char] -> Int -> [Token]
-- If there is no rest, add EOF and stop doing recursive calls.
scanTokensPrivate [] line = [TOKEN EOF "EOF" NONE line]
scanTokensPrivate (x: xs) line
    -- If newline is encountered, recursivley call itself with line + 1.
    | x == '\n' = scanTokensPrivate xs (line + 1)
    -- Ignore whitespace.
    | x == ' ' || x == '\r' || x == '\t' = scanTokensPrivate xs line
    -- Single character lexemes.
    | x == '(' = partAppAddToken LEFT_PAREN
    | x == ')' = partAppAddToken RIGHT_PAREN
    | x == '{' = partAppAddToken LEFT_BRACE
    | x == '}' = partAppAddToken RIGHT_BRACE
    | x == ',' = partAppAddToken COMMA
    | x == '.' = partAppAddToken DOT
    | x == '-' = partAppAddToken MINUS
    | x == '+' = partAppAddToken PLUS
    | x == ';' = partAppAddToken SEMICOLON
    | x == '*' = partAppAddToken STAR
    -- Lexemes with two characters.
    | x == '!' = if match xs '='
        then partAppAddTokenAdvanceTwice BANG_EQUAL
        else partAppAddToken BANG
    | x == '=' = if match xs '=' 
        then partAppAddTokenAdvanceTwice EQUAL_EQUAL
        else partAppAddToken EQUAL
    | x == '<' = if match xs '=' 
        then partAppAddTokenAdvanceTwice LESS_EQUAL
        else partAppAddToken LESS
    | x == '>' = if match xs '=' 
        then partAppAddTokenAdvanceTwice GREATER_EQUAL
        else partAppAddToken GREATER
    -- Lexemes with more than two characters.
    | x == '/' = if match xs '/' 
        -- If dual slashes, it is a comment. Advance until a '\n' is found.
        then scanTokensPrivate (advanceToNewline xs) line
        else partAppAddToken SLASH
    -- If opening character of string literal is encoutred
    | x == '"' = string xs line
    | isDigit x = number (x:xs) line
    | isAlpha x = identifierOrKeyword (x:xs) line
    -- If none of the above match, the syntax is incorrect.
    | otherwise = loxError ("Unexpected character " ++ [x]) line
        where
            -- Advances the list traversal by a single character.
            partAppAddToken = addToken xs line [x] NONE
            -- Advances the list traversal by two characters.
            partAppAddTokenAdvanceTwice = addToken (tail xs) line [x, head xs] NONE

-- Every time a token is added, the scanner continues the recursion.
addToken :: [Char] -> Int -> String -> Literal -> TokenType -> [Token]
addToken xs line lexeme literal tokenType = TOKEN tokenType lexeme literal line : scanTokensPrivate xs line

-- Remove elements from list until \n is found.
advanceToNewline :: [Char] -> [Char]
advanceToNewline = dropWhile (/= '\n')

-- Checks if the first element in given list is the same as a given character.
match :: [Char] -> Char -> Bool
match [] _ = False
match xs character = head xs == character

string :: [Char] -> Int -> [Token]
string xs line =
    let
        stringContents = takeWhile (/= '"') xs
        -- Count the number of newlines in string.
        numberOfLinesInString = count '\n' stringContents
    in
        -- If the length of the remaining list is the same
        -- as the string contents the string must be unterminated.
        if length xs == length stringContents
        then loxError "Unterminated string literal" line
        else
        -- +1 on stringContents length to drop closing '"' from list as well.
        addToken (drop (length stringContents + 1) xs) (line + numberOfLinesInString) stringContents (STR stringContents) STRING

number :: [Char] -> Int -> [Token]
number xs line = 
    let
        numberLit = getNumber xs
        numberValue = read numberLit :: Float
        remainder = drop (length numberLit) xs
    in
        addToken remainder line numberLit (NUM numberValue) NUMBER

getNumber :: [Char] -> [Char]
getNumber [] = []
getNumber (c:rest)
    | isDigit c = c : getNumber rest
    | c == '.' && isDigit (head rest) = c : takeWhile isDigit rest
    | otherwise = []

identifierOrKeyword :: [Char] -> Int -> [Token]
identifierOrKeyword xs line = let
        -- Extract the first lexeme from xs.
        lexeme = takeWhile (\c-> isAlphaNum c || c == '_') xs
        -- Drop the current lexeme from the string of remaining lexemes.
        -- Think of it as advancing the list traversal by length lexeme.
        remainder = drop (length lexeme) xs
        -- Partially apply addToken for more convenient use in case-block.
        partAppAddToken = addToken remainder line lexeme NONE
    in
        case lexeme of 
        "and"    -> partAppAddToken AND
        "class"  -> partAppAddToken CLASS
        "else"   -> partAppAddToken ELSE
        "false"  -> addToken remainder line lexeme FALSE_LIT FALSE
        "for"    -> partAppAddToken FOR
        "fun"    -> partAppAddToken FUN
        "if"     -> partAppAddToken IF
        "nil"    -> addToken remainder line lexeme NIL_LIT NIL
        "or"     -> partAppAddToken OR
        "print"  -> partAppAddToken PRINT
        "return" -> partAppAddToken RETURN
        "super"  -> partAppAddToken SUPER
        "this"   -> partAppAddToken THIS
        "true"   -> addToken remainder line lexeme TRUE_LIT TRUE
        "var"    -> partAppAddToken VAR
        "while"  -> partAppAddToken WHILE
        -- If lexeme does not match any keywords, it is an identifier.
        _ -> identifier xs lexeme line 

identifier :: [Char] -> [Char] -> Int -> [Token]
identifier xs lexeme line =
    let remainder = drop (length lexeme) xs in
        addToken remainder line lexeme (ID lexeme) IDENTIFIER

loxError :: [Char] -> Int -> [Token]
loxError message line = error ("LOX: " ++ message ++ " on line " ++ show line)

count :: Char -> [Char] -> Int
count element list = length (filter (==element) list)