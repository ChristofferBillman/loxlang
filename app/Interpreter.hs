{-
    LOX Interpreter
    Version: 2022-03-XX
    Author: Christoffer Billman
-}

{-
    TODO:
    * Report what line an error occured on.
-}
module Interpreter (interpret) where

import Expr
import Stmt
import Tokens
import Data.Maybe

type Environment = [(String,Value)]

data Value = StrVal {getStrVal :: String}
          | NumVal  {getNumVal :: Float}
          | NilVal
          | TrueVal
          | FalseVal
        deriving (Show)
          
instance Num Value where
  (+) (NumVal n1) (NumVal n2) = NumVal (n1+n2)
  (+) (StrVal s1) (StrVal s2) = StrVal (s1++s2)
  (+) _ _ = loxError "Use of '+' operator on non- string or numeric value"
  (*) (NumVal n1) (NumVal n2) = NumVal (n1*n2)
  (*) _ _ = loxError "Use of '*' operator on non-numeric value"
  (-) (NumVal n1) (NumVal n2) = NumVal (n1-n2)
  (-) _ _ = loxError "Use of binary '-' operator on non-numeric value"

instance Ord Value where
    (>) (NumVal n1) (NumVal n2) = n1 > n2
    (>) _ _ = loxError "Use of '>' operator on non-numeric value"
    (>=) (NumVal n1) (NumVal n2) = n1 >= n2
    (>=) _ _ = loxError "Use of '>=' operator on non-numeric value"
    (<) (NumVal n1) (NumVal n2) = n1 < n2
    (<) _ _ = loxError "Use of '<' operator on non-numeric value"
    (<=) (NumVal n1) (NumVal n2) = n1 <= n2
    (<=) _ _ = loxError "Use of '<=' operator on non-numeric value"

instance Eq Value where
    (==) (NumVal n1) (NumVal n2) = n1 == n2
    (==) (StrVal s1) (StrVal s2) = s1 == s2
    (==) TrueVal TrueVal         = True
    (==) FalseVal FalseVal       = True
    (==) NilVal NilVal           = True
    (==) _ _                     = False

    (/=) (NumVal n1) (NumVal n2) = n1 /= n2
    (/=) (StrVal s1) (StrVal s2) = s1 /= s2
    (/=) TrueVal TrueVal         = False
    (/=) FalseVal FalseVal       = False
    (/=) NilVal NilVal           = False
    (/=) _ _                     = True

instance Fractional Value where
(/) (NumVal n1) (NumVal n2) = NumVal (n1 Prelude./ n2)


interpret :: [Stmt] -> [String]
interpret = map show

execute :: Stmt -> String
execute (ExprStmt expr)  = printResult $ evaluate expr
execute (PrintStmt expr) = printResult $ evaluate expr
execute (VarDeclStmt)    = printResult $ define

-- Takes a variable declaration statement, and an environment.
-- Adds the variable from the declration to the enivironment.
-- Returns the new environment.
define :: Stmt -> Environment -> Environment
define (VarDeclStmt token (Just initializer)) env = (getTokenStr token, evaluate initializer) : env 
define (VarDeclStmt token Nothing) env            = (getTokenStr token, NilVal) : env
define _ _ = error "Internal Error: Passed a statement that is not a variable declaration to define."

evaluate :: Expr -> Value
evaluate (Literal token) = toValFromLit $ getTokenLit token
evaluate (Grouping expr) = evaluate expr
evaluate (Unary opr expr)
    | tt == MINUS = if isNumeric right
                    then -right
                    else loxError "Use of unary '-' operator on non-numeric value"
    | tt == BANG =  if isTruthy right
                    then FalseVal
                    else TrueVal
    where
        right = evaluate expr
        tt = getTokenType opr

evaluate (Binary left opr right)
    | tt == MINUS         = leftVal - rightVal
    | tt == SLASH         = leftVal Interpreter./ rightVal
    | tt == STAR          = leftVal * rightVal
    | tt == PLUS          = leftVal + rightVal
    | tt == GREATER       = toVal (leftVal >  rightVal)
    | tt == GREATER_EQUAL = toVal (leftVal >= rightVal)
    | tt == LESS          = toVal (leftVal <  rightVal)
    | tt == LESS_EQUAL    = toVal (leftVal <= rightVal)
    | tt == BANG_EQUAL    = toVal (leftVal /= rightVal)
    | tt == EQUAL_EQUAL   = toVal (leftVal == rightVal)
    where
        tt       = getTokenType opr
        leftVal  = evaluate left
        rightVal = evaluate right
evaluate _ = loxError "An unknown error occured while evaluating expression"

printResult :: Value -> String
printResult TrueVal  = "true"
printResult FalseVal = "false"
printResult NilVal   = "nil"
printResult (NumVal val) = show val
printResult (StrVal val) = show val

-- Tells you whether or not a given Value is numeric or not.
isNumeric :: Value -> Bool
isNumeric (NumVal _) = True
isNumeric _ = False

-- Tells you whether a Value is truthy of falsey.
-- Everything but NilVal and FalseVal is considered truthy.
isTruthy :: Value -> Bool
isTruthy NilVal   = False
isTruthy FalseVal = False
isTruthy _        = True

toVal :: Bool -> Value
toVal bool 
    | bool      = TrueVal
    | otherwise = FalseVal

toValFromLit :: Literal -> Value
toValFromLit TRUE_LIT  = TrueVal
toValFromLit FALSE_LIT = FalseVal
toValFromLit NIL_LIT   = NilVal
toValFromLit (NUM val) = NumVal val
toValFromLit (STR val) = StrVal val
toValFromLit _         = loxError "Invalid literal encountered"

loxError :: String -> error
loxError message = error $ "LOX Runtime Error: " ++ message ++ "."