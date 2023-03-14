module Expr where
import Tokens
import Data.List (intercalate)

data Expr = Literal  Token
          | Unary    Token Expr
          | Binary   Expr Token Expr
          | Call     Expr Token [Expr]
          | Grouping Expr
          | Variable Token
          | Assign   Token Expr

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
    show (Call callee _ args) = show callee ++ "(" ++ intercalate "," (map show args)  ++ ")"

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