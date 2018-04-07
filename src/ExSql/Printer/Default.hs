{-# LANGUAGE TypeOperators, GADTs, RankNTypes, KindSignatures #-}
module ExSql.Printer.Default
    ( Printer(..)
    , pretty
    , prettyLiteral
    , prettyArithmetic
    , prettyComparison
    , prettyLogical
    ) where

import Data.Extensible ((:*), (:|)(..), hindex)
import Data.Functor.Identity (Identity(..))
import Data.Text (Text)
import ExSql.Syntax.Class (Expr(..), Node(..))
import ExSql.Syntax.Literal
import ExSql.Syntax.Arithmetic
import ExSql.Syntax.Comparison
import ExSql.Syntax.Logical

newtype Printer (g :: * -> *) (v :: (* -> *) -> * -> *) = Printer { runPrinter :: forall a. v g a -> String }

pretty :: Printer (Expr xs) :* xs -> Expr xs a -> String
pretty printers (Expr (EmbedAt membership (Node a))) = runPrinter (hindex printers membership) a

prettyLiteral :: Literal g a -> String
prettyLiteral (LitInt a) = show a
prettyLiteral (LitBool a) = show a

prettyComparison :: (forall b. Expr xs b -> String) -> Comparison (Expr xs) a -> String
prettyComparison p (Equality a0 a1) = "(" ++ p a0 ++ "==" ++ p a1 ++ ")"
prettyComparison p (GreaterThan a0 a1) = "(" ++ p a0 ++ ">" ++ p a1 ++ ")"
prettyComparison p (LessThan a0 a1) = "(" ++ p a0 ++ "<" ++ p a1 ++ ")"
prettyComparison p (GreaterThanOrEqual a0 a1) = "(" ++ p a0 ++ ">=" ++ p a1 ++ ")"
prettyComparison p (LessThanOrEqual a0 a1) = "(" ++ p a0 ++ "<=" ++ p a1 ++ ")"

prettyArithmetic :: (forall b. Expr xs b -> String) -> Arithmetic (Expr xs) a -> String
prettyArithmetic p (Negation a) = "(-" ++ p a ++ ")"
prettyArithmetic p (Addition a0 a1) = "(" ++ p a0 ++ "+" ++ p a1 ++ ")"
prettyArithmetic p (Subtraction a0 a1) = "(" ++ p a0 ++ "-" ++ p a1 ++ ")"
prettyArithmetic p (Multiplication a0 a1) = "(" ++ p a0 ++ "*" ++ p a1 ++ ")"
prettyArithmetic p (Division a0 a1) = "(" ++ p a0 ++ "/" ++ p a1 ++ ")"

prettyLogical :: (forall b. Expr xs b -> String) -> Logical (Expr xs) a -> String
prettyLogical p (LogicalNegation a) = "(!" ++ p a ++ ")"
prettyLogical p (Conjunction a0 a1) = "(" ++ p a0 ++ "&&" ++ p a1 ++ ")"
prettyLogical p (Disjunction a0 a1) = "(" ++ p a0 ++ "||" ++ p a1 ++ ")"
