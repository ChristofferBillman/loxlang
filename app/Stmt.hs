{-
    TODO:
    * Replace record syntax with normal syntax.
-}
module Stmt where
import Tokens
import Expr
import Data.Maybe (fromJust)
import Data.List (intercalate)

data Stmt = ExprStmt    Expr
          | PrintStmt   Expr
          | ReturnStmt  Token Expr
          | VarDeclStmt Token (Maybe Expr)
          | FunDeclStmt Token [Token] Stmt
          | BlockStmt   [Stmt]
          | IfStmt      Expr Stmt (Maybe Stmt)
          | WhileStmt   Expr Stmt

instance Show Stmt where
    show (ExprStmt expr) = show expr ++ ";"
    show (PrintStmt val) = "print " ++ show val ++ ";"
    show (ReturnStmt keyword (Literal (TOKEN _ _ NIL_LIT _))) = "return;"
    show (ReturnStmt keyword value) = "return " ++ show value ++ ";"
    show (VarDeclStmt token Nothing) = "V DEC -> " ++ getTokenStr token ++ ";"
    show (VarDeclStmt token init) = "V DEC -> " ++ getTokenStr token ++ "=" ++ show (fromJust init) ++ ";"
    show (FunDeclStmt (TOKEN _ _ (ID name) _) params block) = "F DEC -> " ++ name ++ "(" ++ intercalate "," (map getTokenStr params) ++ ")" ++ show block 
    show (BlockStmt stmts) = "{" ++ concatMap show stmts ++ "}"
    show (IfStmt condition thenB Nothing) = "if" ++ show condition ++ show thenB
    show (IfStmt condition thenB elseB) = "if" ++ show condition ++ show thenB ++ " else " ++ show (fromJust elseB)
    show (WhileStmt condition body) = "while " ++ show condition ++ show body
