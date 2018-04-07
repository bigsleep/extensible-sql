{-# LANGUAGE TypeOperators, GADTs #-}
module ExSql.Printer.Default
    ( pretty
    , prettyLiteral
    , prettyArithmetic
    , prettyLogical
    ) where

import Data.Extensible ((:*), Match, match)
import Data.Text (Text)
import ExSql.Syntax.Class (Expr(..), Node)
import ExSql.Syntax.Literal
import ExSql.Syntax.Arithmetic
import ExSql.Syntax.Logical

pretty :: Match (Node (Expr xs) a) r :* xs -> Expr xs a -> r
pretty printers (Expr expr) = match printers expr

prettyLiteral :: Literal g a -> String
prettyLiteral (LitInt a) = show a
prettyLiteral (LitBool a) = show a

prettyArithmetic :: (Expr xs a -> String) -> Arithmetic (Expr xs) a -> String
prettyArithmetic p (Negation a) = "(-" ++ p a ++ ")"
prettyArithmetic p (Addition a0 a1) = "(" ++ p a0 ++ "+" ++ p a1 ++ ")"
prettyArithmetic p (Subtraction a0 a1) = "(" ++ p a0 ++ "-" ++ p a1 ++ ")"
prettyArithmetic p (Multiplication a0 a1) = "(" ++ p a0 ++ "*" ++ p a1 ++ ")"
prettyArithmetic p (Division a0 a1) = "(" ++ p a0 ++ "/" ++ p a1 ++ ")"

prettyLogical :: (Expr xs a -> String) -> Logical (Expr xs) a -> String
prettyLogical p (LogicalNegation a) = "(!" ++ p a ++ ")"
prettyLogical p (Conjunction a0 a1) = "(" ++ p a0 ++ "&&" ++ p a1 ++ ")"
prettyLogical p (Disjunction a0 a1) = "(" ++ p a0 ++ "||" ++ p a1 ++ ")"
