{-# LANGUAGE DataKinds, FlexibleContexts, TypeOperators, RankNTypes #-}
module Main where

import Data.Text (Text)
import qualified Data.Text as Text (unpack)
import Data.Extensible (Member, Match(..), (:*), (<:), nil)
import ExSql.Syntax.Arithmetic
import ExSql.Syntax.Comparison
import ExSql.Syntax.Logical
import ExSql.Syntax.Literal
import ExSql.Syntax.Class
import ExSql.Syntax.Relativity
import ExSql.Printer.Default

e1 :: (Member xs Comparison, Member xs Arithmetic, Member xs Literal, Member xs Logical) => Expr xs Bool
e1 = disjunction (bool False) (conjunction (equality (addition (multiplication (int 1) (negation (int 2))) (division (int 3) (addition (int 4) (int 2)))) (multiplication (int 5) (int 2))) (bool True))

type Nodes = '[Logical, Comparison, Arithmetic, Literal]
type E = Expr Nodes
type Printers xs a = Printer (Expr xs) :* xs

printers :: (forall b. Maybe Relativity -> Maybe Relativity -> Expr Nodes b -> Text) -> Printers Nodes a
printers p
    =  Printer (prettyLogical p)
    <: Printer (prettyComparison p)
    <: Printer (prettyArithmetic p)
    <: Printer (prettyLiteral)
    <: nil

pp :: Maybe Relativity -> Maybe Relativity -> Expr Nodes a -> Text
pp = pretty (printers pp)

main :: IO ()
main = do
    putStrLn . Text.unpack $ pp Nothing Nothing e1
    putStrLn "hello"
