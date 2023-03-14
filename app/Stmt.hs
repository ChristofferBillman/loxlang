{-
    TODO:
    * Replace record syntax with normal syntax.
-}
module Stmt where
import Tokens
import Expr
import Data.Maybe (fromJust)
import Data.List (intercalate)

data Stmt = ExprStmt {getExpr :: Expr }
          | PrintStmt {getPrintExpr :: Expr }
          | VarDeclStmt {getVarToken :: Token, getInitializer :: Maybe Expr }
          | FunDeclStmt {getFunName :: Token, getFunParams :: [Token], getFunBody :: Stmt}
          | BlockStmt {getBlockStmts :: [Stmt]}
          | IfStmt {getIfCondition :: Expr, getThenBranch :: Stmt, getElseBranch ::  Maybe Stmt }
          | WhileStmt {getWhileCondition :: Expr, getWhileBody :: Stmt}

instance Show Stmt where
    show (ExprStmt expr) = show expr ++ ";"
    show (PrintStmt val) = "print " ++ show val ++ ";"
    show (VarDeclStmt token Nothing) = "V DEC -> " ++ getTokenStr token ++ ";"
    show (VarDeclStmt token init) = "V DEC -> " ++ getTokenStr token ++ "=" ++ show (fromJust init) ++ ";"
    show (FunDeclStmt (TOKEN _ _ (ID name) _) params block) = "F DEC -> " ++ name ++ "(" ++ intercalate "," (map getTokenStr params) ++ ")" ++ show block 
    show (BlockStmt stmts) = "{" ++ concatMap show stmts ++ "}"
    show (IfStmt condition thenB Nothing) = "if" ++ show condition ++ show thenB
    show (IfStmt condition thenB elseB) = "if" ++ show condition ++ show thenB ++ " else " ++ show (fromJust elseB)
    show (WhileStmt condition body) = "while " ++ show condition ++ show body
