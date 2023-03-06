{-
    LOX Interpreter
    Version: 2022-03-XX
    Author: Christoffer Billman
-}
module Interpreter (interpret) where

import Parser (Stmt)

interpret :: [Stmt] -> [String]
interpret = map show