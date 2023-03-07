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

data Environment = Environment [(String,Value)] (Maybe Environment) deriving (Show)

data Value = StrVal {getStrVal :: String}
          | NumVal  {getNumVal :: Float}
          | NilVal
          | TrueVal
          | FalseVal

instance Show Value where
    show (StrVal str) = str
    show (NumVal num) = show num
    show NilVal     = "nil"
    show TrueVal     = "true"
    show FalseVal     = "false"
          
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
-- TODO: Fix problem where things are interpreted the wrong way around (reverse is quick fix).
interpret program = reverse $ executeProgram (program, Environment [] Nothing, [])

executeProgram :: ([Stmt], Environment, [String]) -> [String]
executeProgram (stmt:rest, env, out) 
    | null rest = out'
    | otherwise        = executeProgram (rest, env', out')
    where
        (env', out')   = execute (stmt, env, out)

-- Executes a single statement, and returns the effects of its execution,
-- i.e. a variable change (change in environment) or a printout.
execute :: (Stmt, Environment, [String]) -> (Environment, [String])
execute (ExprStmt expr, env, out) = (newEnv, out)
    where
        (value, newEnv) = evaluate (expr, env)

execute (PrintStmt expr, env, out) = (newEnv, show value : out)
    where
        (value, newEnv) = evaluate (expr, env)

execute (stmt@(VarDeclStmt{}), env, out) = (define (stmt, env), out)

-- Takes a variable declaration statement, and an environment.
-- Adds the variable from the declration to the environment.
-- Returns the new environment.
define :: (Stmt, Environment) -> Environment
-- Case where variable is assigned a value.
-- Confusing, but this pattern match extracts the ID from the var decl.
define (VarDeclStmt (TOKEN _ _ (ID id) _) (Just initializer), Environment current outer) = Environment (entry : current) outer
    where
        entry = (id, init)
        (init, _) = evaluate (initializer, Environment current outer)

-- Case where no value is assigned to var.
-- Confusing, but this pattern match extracts the ID from the var decl.
define (VarDeclStmt (TOKEN _ _ (ID id) _) Nothing, Environment current outer) = Environment (entry : current) outer
    where
        entry = (id, NilVal)
define _ = error "Internal Error: Passed a statement that is not a variable declaration to define."

-- Looks a variable with a name exists, if it does it returns its value.
-- Recursivley calls itself to search though all enclosing environments.
get :: String -> Environment -> Maybe Value
get key (Environment current outer)
    | null current && isNothing outer = Nothing
    | isJust value                    = value
    | isNothing outer                 = Nothing
    | otherwise                       = get key (fromJust outer)
        where
            value = lookup key current

assign :: (String, Value) -> Environment -> Environment
assign (name, value) (Environment current outer)
    | isNothing result = loxError $ "Cannot assign value to undefined variable: " ++ name
    | otherwise        = Environment ((name, fromJust result) : current) outer
    where
        result = get name (Environment current outer)

evaluate :: (Expr, Environment) -> (Value, Environment)
evaluate (Literal token,  env) = (toValFromLit $ getTokenLit token, env)
evaluate (Grouping expr,  env) = evaluate (expr, env)
evaluate (Unary opr expr, env)
    | tt == MINUS = if isNumeric right
                    then (-right, newEnv)
                    else loxError "Use of unary '-' operator on non-numeric value"
    | tt == BANG =  if isTruthy right
                    then (FalseVal, newEnv)
                    else (TrueVal , newEnv)
    where
        (right, newEnv) = evaluate (expr, env)
        tt = getTokenType opr
evaluate (Binary left opr right, env)
    | tt == MINUS         = (leftVal - rightVal, newEnv)
    | tt == SLASH         = (leftVal Interpreter./ rightVal, newEnv)
    | tt == STAR          = (leftVal * rightVal, newEnv)
    | tt == PLUS          = (leftVal + rightVal, newEnv)
    | tt == GREATER       = (toVal (leftVal >  rightVal), newEnv)
    | tt == GREATER_EQUAL = (toVal (leftVal >= rightVal), newEnv)
    | tt == LESS          = (toVal (leftVal <  rightVal), newEnv)
    | tt == LESS_EQUAL    = (toVal (leftVal <= rightVal), newEnv)
    | tt == BANG_EQUAL    = (toVal (leftVal /= rightVal), newEnv)
    | tt == EQUAL_EQUAL   = (toVal (leftVal == rightVal), newEnv)
    where
        tt       = getTokenType opr
        (leftVal, leftEnv)  = evaluate (left, env)
        (rightVal, newEnv) = evaluate (right, leftEnv)
    
-- TODO: Call get with literal instead of str.
evaluate (Variable (TOKEN _ str _ _), env) 
    | isNothing result = loxError $ "Variable " ++ str ++ " not defined"
    | otherwise        = (fromJust result, env)
    where
        result = get str env
evaluate (Assign (TOKEN _ str _ _) expr, env) = (value, assign (str, value) newEnv)
    where
        (value, newEnv) = evaluate (expr,env)
evaluate _ = loxError "An unknown error occured while evaluating expression"

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