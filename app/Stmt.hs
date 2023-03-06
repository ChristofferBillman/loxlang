module Stmt where

import Tokens
import Expr
import Data.Maybe (fromJust)

data Stmt = ExprStmt {getExpr :: Expr }
          | PrintStmt {getPrintExpr :: Expr }
          | VarDeclStmt {getVarToken :: Token, getInitializer :: Maybe Expr }
          | BlockStmt {getBlockStmts :: [Stmt]}
          | IfStmt {getIfCondition :: Expr, getThenBranch :: Stmt, getElseBranch ::  Maybe Stmt }
          | WhileStmt {getWhileCondition :: Expr, getWhileBody :: Stmt}

instance Show Stmt where
    show (ExprStmt expr) = show expr ++ ";"
    show (PrintStmt val) = "print " ++ show val ++ ";"
    show (VarDeclStmt token Nothing) = "V DEC -> " ++ getTokenStr token ++ ";"
    show (VarDeclStmt token init) = "V DEC -> " ++ getTokenStr token ++ "=" ++ show (fromJust init) ++ ";"
    show (BlockStmt stmts) = "{" ++ concatMap show stmts ++ "}"
    show (IfStmt condition thenB Nothing) = "if" ++ show condition ++ show thenB
    show (IfStmt condition thenB elseB) = "if" ++ show condition ++ show thenB ++ " else " ++ show (fromJust elseB)
    show (WhileStmt condition body) = "while " ++ show condition ++ show body
