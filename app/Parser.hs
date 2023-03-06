{-
    LOX Parser
    Version: 2022-03-03
    Author: Christoffer Billman

    Changes version 2022-03-XX
    * Prefixed all record functions with 'get'.
-}
module Parser (parse, Stmt) where
import Tokens
import Data.Typeable ( typeOf )
import Prelude hiding (and, or)
import Data.Maybe (isJust, fromJust)

data Expr = Literal {getLiteral :: Token }
          | Unary {getUnaryOpr :: Token, getUnaryExpr :: Expr }
          | Binary { getBinaryLeft :: Expr, getBinaryOpr :: Token, getBinaryRight :: Expr }
          | Grouping { getGrpExpr :: Expr }
          | Variable {getVar :: Token }
          | Assign {getVarAssign :: Token, getAssignExpr :: Expr }
data Stmt = ExprStmt {getExpr :: Expr }
          | PrintStmt {getPrintExpr :: Expr }
          | VarDeclStmt {getVarToken :: Token, getInitializer :: Maybe Expr }
          | BlockStmt {getBlockStmts :: [Stmt]}
          | IfStmt {getIfCondition :: Expr, getThenBranch :: Stmt, getElseBranch ::  Maybe Stmt }
          | WhileStmt {getWhileCondition :: Expr, getWhileBody :: Stmt}
instance Show Expr where
    show (Literal token)
        | tt == TRUE || tt == FALSE || tt == NIL = show $ getTokenLit token
        | tt == STRING = "\"" ++ getTokenStr token ++ "\""
        | tt == NUMBER = getLiteralVal literal 
        | otherwise = getTokenStr token
        where
            literal = getTokenLit token
            tt = getTokenType token
    show (Unary opr expr) = "(" ++ getTokenStr opr ++ show expr ++ ")"
    show (Binary left opr right) 
        | getTokenType opr == AND = "(" ++ show left ++ " && " ++ show right ++ ")"
        | getTokenType opr == OR = "(" ++ show left ++ " || " ++ show right ++ ")"
        | otherwise = "(" ++ show left ++ " " ++ getTokenStr opr ++ " " ++ show right ++ ")"
    show (Grouping expr)     = "(" ++ show expr ++ ")"
    show (Assign token expr) = getTokenStr token ++ "=" ++ show expr
    show (Variable token)    = getTokenStr token

instance Show Stmt where
    show (ExprStmt expr) = show expr ++ ";"
    show (PrintStmt val) = "print " ++ show val ++ ";"
    show (VarDeclStmt token Nothing) = "V DEC -> " ++ getTokenStr token ++ ";"
    show (VarDeclStmt token init) = "V DEC -> " ++ getTokenStr token ++ "=" ++ show (fromJust init) ++ ";"
    show (BlockStmt stmts) = "{" ++ concatMap show stmts ++ "}"
    show (IfStmt condition thenB Nothing) = "if" ++ show condition ++ show thenB
    show (IfStmt condition thenB elseB) = "if" ++ show condition ++ show thenB ++ " else " ++ show (fromJust elseB)
    show (WhileStmt condition body) = "while " ++ show condition ++ show body

-- Program is an alias for a list of statements.
newtype Program = Program [Stmt]
instance Show Program where
    show (Program decs) = show (length decs) ++ "\n" ++ unlines (map show decs)

-- Parses a list of tokens into a list of statements.
parse :: [Token] -> [Stmt]
parse [] = error "INTERNAL ERROR: List of tokens provided was empty. Provide a non-empty list of tokens."
parse tokens = program tokens

-- Handles the recursion for the program. Runs statement repeated amount of times.
program :: [Token] -> [Stmt]
program tokens
    -- Return program if EOF token is encountered.
    | isAtEnd tokens = []
    | otherwise      = prgm : program rest
    where
        (prgm, rest) = declaration tokens

declaration :: [Token] -> (Stmt, [Token])
declaration tokens
    -- Consume VAR token.
    | match tokens [VAR] = variableDeclaration $ tail tokens
    | otherwise          = statement tokens

variableDeclaration :: [Token] -> (Stmt, [Token])
variableDeclaration tokens
    -- Do it on the tail of tokens to move past the identifier.
    -- Error if no identifier follows a var token.
    | not $ match tokens [IDENTIFIER] = loxError "Expected variable name" (head tokens)
    -- If the token after the identifier is EQUALS, we know that an initialization expression will follow.
    | match (tail tokens) [EQUAL]     = (VarDeclStmt (head tokens) (Just initializer), checkedRest)
    -- If the token after the identifier is semicolon, a variable is only declared, not initialized.
    | match (tail tokens) [SEMICOLON] = (VarDeclStmt (head tokens) Nothing, tail $ tail tokens)
    -- If none of the above, we assert that a semicolon is expected.
    | otherwise = loxError "Expected ';' after variable declaration" (head tokens)
    where
        (initializer, rest) 
            | isAtEnd (tail $ tail tokens) = loxError "Expected expression after '=' in variable declaration" (head tokens)
            | otherwise = expression $ tail $ tail tokens
        (_, checkedRest) = if match rest [SEMICOLON]
                           -- Consume last semicolon.
                           then (initializer, tail rest)
                           else loxError "Expected ';' after variable declaration" (head tokens)

{- STATEMENTS -}

statement :: [Token] -> (Stmt, [Token])
statement tokens
    | isAtEnd tokens            = loxError "Unexpected end of input" (head tokens)
    | match tokens [FOR]        = forStatement tokens
    | match tokens [IF]         = ifStatement tokens
    | match tokens [PRINT]      = printStatement tokens
    | match tokens [WHILE]      = whileStatement tokens
    -- Consume the left brace token with tail tokens.
    | match tokens [LEFT_BRACE] = (BlockStmt block, rest)
    | otherwise                 = expressionStatement tokens
    where
        (block, rest) = blockStatement $ tail tokens

ifStatement :: [Token] -> (Stmt, [Token])
ifStatement tokens
    -- Check opening paren before conditon
    | not $ match firstParen [LEFT_PAREN] = loxError "Expected '(' after 'if'" (head tokens)
    | otherwise = (IfStmt condition thenBranch elseBranch, elseRest)
    where
        -- consume IF-token
        firstParen = tail tokens
        -- Get the condition
        -- Important that opening parenthesis is included so expression
        -- parses it as a group.
        (condition, conditionRest) = expression firstParen
        (thenBranch, thenRest)     = statement conditionRest
        (elseBranch, elseRest)     = if match thenRest [ELSE]
                                     then (Just elseBranch', elseRest')
                                     else (Nothing, thenRest)
        (elseBranch', elseRest')   = statement $ tail thenRest

printStatement :: [Token] -> (Stmt, [Token])
printStatement tokens
    -- Tail on rest is important because it consumes the semicolon.
    | match rest [SEMICOLON] = (PrintStmt expr, tail rest)
    | otherwise = loxError "Expected ';' after value" (head tokens)
    where
        -- Consume PRINT token
        (expr, rest) = expression (tail tokens)

whileStatement :: [Token] -> (Stmt, [Token])
whileStatement tokens
    | not $ match firstParen [LEFT_PAREN] = loxError "Expected '(' after 'while'" (head tokens)
    | otherwise = (WhileStmt condition body, bodyRest)
    where
        -- Consume WHILE token
        firstParen = tail tokens
        (condition, conditionRest) = expression firstParen
        (body, bodyRest) = statement conditionRest

-- NOTE: The code for extracting the initializer, condition and increment is
--       very similar and could probably be extracted into a seperate function.
forStatement :: [Token] -> (Stmt, [Token])
forStatement tokens
    -- If there is no '(' after token FOR.
    | not $ match firstParen [LEFT_PAREN] = loxError "Expected '(' after 'for'" (head tokens)
    -- If there is an initializer, add it at the begining of a block statement containing
    -- the rest of the loop.
    | isJust initializer = (BlockStmt [fromJust initializer, whileBody], bodyRest)
    | otherwise = (whileBody, bodyRest)
    where
        firstParen = tail tokens
        -- Handle initializer.
        -- Remove var token, hence double tail.
        (varDecl,varDeclRest) = variableDeclaration (tail $ tail firstParen)
        (expr, exprRest)      = expressionStatement (tail firstParen)
        (initializer, initializerRest)
            -- No initializer.
            | match (tail firstParen) [SEMICOLON] = (Nothing, tail firstParen)
            -- Variable declaration.
            | match (tail firstParen) [VAR]       = (Just varDecl, varDeclRest)
            -- Expression.
            | otherwise                           = (Just expr, exprRest)

        -- Handle condition
        (condition', conditionRest') = expression initializerRest
        (condition, conditionRest)
            -- If there is no condition statement provided, set its condition to true.
            | match initializerRest [SEMICOLON] = (Literal (TOKEN TRUE "true" TRUE_LIT 0), tail initializerRest)
            -- If there is no semicolon after the condition, it is a syntax error.
            | not $ match conditionRest' [SEMICOLON]  = loxError "Expected ';' after loop condition" (head conditionRest')
            | otherwise = (condition', tail conditionRest')

        -- Handle increment
        (increment', incrementRest') = expression conditionRest
        (increment, incrementRest)
            -- No increment expression.
            | match conditionRest [RIGHT_PAREN] = (Nothing, tail conditionRest)
            -- If there is an increment expression.
            | match incrementRest' [RIGHT_PAREN] = (Just increment', incrementRest')
            -- If none of the above match, a ')' is missing.
            | otherwise = loxError "Expected ')' after for clauses" (head conditionRest)

        -- If there is an increment-part, add it at the end of the while-block, else dont do anything with the body.
        -- Consume closing ')' with tail incrementRest.
        (body',bodyRest') = statement $ tail incrementRest
        (body, bodyRest)
            | isJust increment = (BlockStmt [body', ExprStmt $ fromJust increment], bodyRest')
            | otherwise        = (body',bodyRest')

        whileBody = WhileStmt condition body

blockStatement :: [Token] -> ([Stmt], [Token])
blockStatement tokens
    | isAtEnd tokens = loxError "Expected '}'" (head tokens)
    | not $ match tokens [RIGHT_BRACE] = (decl : stmts, accRest)
    | otherwise = ([], tail tokens)
    where
        (decl, rest) = declaration tokens
        (stmts, accRest) = blockStatement rest

expressionStatement :: [Token] -> (Stmt, [Token])
expressionStatement tokens
    -- Tail on rest is important because it consumes the semicolon.
    | match rest [SEMICOLON] = (ExprStmt expr, tail rest)
    | otherwise = loxError "Expected ';' after value" (head tokens)
    where
        (expr, rest) = expression tokens

{- EXPRESSIONS -}

expression :: [Token] -> (Expr, [Token])
expression tokens
    | isAtEnd tokens = error "LOX: Unexpected end of input"
    | otherwise = assignment tokens

assignment :: [Token] -> (Expr, [Token])
assignment tokens
    | match rest [EQUAL] = if isVariable expr
                           then (Assign (head tokens) value, valueRest)
                           else loxError "Invalid assignment target" (head tokens)
    | otherwise          = (expr, rest)
    where
        (expr, rest)       = or tokens
        equals             = head rest
        -- tail rest consumes the EQUAL token.
        (value, valueRest) = assignment (tail rest)

or :: [Token] -> (Expr, [Token])
or tokens
    | isAtEnd tokens = error "LOX: Unexpected end of input"
    | otherwise      = binary rest leftExpr [OR] and
    where
        (leftExpr, rest) = and tokens

and :: [Token] -> (Expr, [Token])
and tokens = binary rest leftExpr [AND] equality
    where
        (leftExpr, rest) = equality tokens

equality :: [Token] -> (Expr, [Token])
equality tokens = binary rest leftExpr [EQUAL_EQUAL, BANG_EQUAL] comparison
    where
        (leftExpr, rest) = comparison tokens

comparison :: [Token] -> (Expr, [Token])
comparison tokens = binary rest expr [GREATER, GREATER_EQUAL, LESS, LESS_EQUAL] term
    where
        (expr, rest) = term tokens

term :: [Token] -> (Expr, [Token])
term tokens = binary rest expr [MINUS, PLUS] factor
    where
        (expr, rest) = factor tokens

factor :: [Token] -> (Expr, [Token])
factor tokens = binary rest expr [SLASH, STAR] unary
    where
        (expr, rest) = unary tokens

unary :: [Token] -> (Expr, [Token])
unary tokens
    | match tokens [BANG, MINUS] = (Unary operator right, rightRest)
    -- Take out the consumed primary token from the tokens list.
    | otherwise = primary tokens
    where
        operator           = head tokens
        (right, rightRest) = if isAtEnd $ tail tokens
                             then loxError "Missing right side of unary expression" operator
                             else unary $ tail tokens

primary :: [Token] -> (Expr, [Token])
primary tokens
    | null tokens = error "INTERNAL ERROR: Primary was invoked with an empty list of tokens."
    | match tokens [FALSE, TRUE, NIL, NUMBER, STRING, EOF] = (Literal (head tokens), tail tokens)
    | match tokens [IDENTIFIER]  = (Variable (head tokens), tail tokens)
    | match tokens [LEFT_PAREN]  = if match rest [RIGHT_PAREN]
                                   -- Remove ')' first in rest.
                                   then (grouping, tail rest)
                                   else loxError "Expected ')' after expression" (head tokens)
    | match tokens [RIGHT_PAREN] = loxError "Expected '(' before ')'" (head tokens)
    | otherwise                  = loxError "Not an expression" (head tokens)
    where
        -- Remove first '(' with tail tokens
        (groupingExpr, rest) = expression (tail tokens)
        grouping = Grouping groupingExpr
-- Takes a list of tokens, its left hand expression, operators to match for
-- and a function to parse the given tokens with.
binary :: [Token] -> Expr -> [TokenType] -> ([Token] -> (Expr, [Token])) -> (Expr, [Token])
binary tokens leftExpr tokenTypes descend
    -- Case where next token does match, recursivley call itself with
    -- newly formed binary expression as left side in new binary expression.
    | match tokens tokenTypes = binary rest (Binary leftExpr operator right) tokenTypes descend
    -- Case where next token does not match any TokenType provided,
    -- continue parsing expression with provided descend function.
    | otherwise = (leftExpr, tokens)
    where
        -- Extract operator. Should be the first element in list.
        operator      = head tokens
        -- Extract right-hand side of binary expression, with the operator taken out.
        -- Left-hand side is already consumed from list of tokens by binary's caller.
        (right, rest) = if isAtEnd $ tail tokens
                        then loxError "Missing right side of binary expression" operator
                        else descend $ tail tokens

{- HELPER FUNCTIONS -}

match :: [Token] -> [TokenType] -> Bool
-- check is being partially applied here
-- new function returned is with signature TokenType -> Bool.
match tokens = any (check tokens)

-- Checks whether the TokenType of first token in given list
-- is the same as the given TokenType.
check :: [Token] -> TokenType -> Bool
check tokens tokenType = getTokenType (head tokens) == tokenType

-- If the first token in given list is of type EOF
-- return True, else False.
isAtEnd :: [Token] -> Bool
isAtEnd tokens = getTokenType (head tokens) == EOF

isVariable :: Expr -> Bool
isVariable (Variable _) = True
isVariable expr         = False

getTokenType :: Token -> TokenType
getTokenType (TOKEN tokenType _ _ _) = tokenType

getTokenLit :: Token -> Tokens.Literal
getTokenLit (TOKEN _ _ literal _) = literal

getTokenLine :: Token -> Int
getTokenLine (TOKEN _ _ _ line) = line

getTokenStr :: Token -> String
getTokenStr (TOKEN _ str _ _) = str

getLiteralVal :: Literal -> String
getLiteralVal (NUM val) = show val
getLiteralVal literal   = show literal

loxError :: [Char] -> Token -> error
loxError message token = error ("LOX: " ++ message ++ " on line " ++ show (getTokenLine token) ++ " around " ++ show (getTokenStr token))
