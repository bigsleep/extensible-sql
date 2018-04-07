{-# LANGUAGE DataKinds, FlexibleContexts, TypeOperators, RankNTypes #-}
module Main where

import Data.Extensible (Member, Match(..), (:*), (<:), nil)
import ExSql.Syntax.Arithmetic
import ExSql.Syntax.Comparison
import ExSql.Syntax.Literal
import ExSql.Syntax.Class
import ExSql.Printer.Default

e1 :: (Member xs Comparison, Member xs Arithmetic, Member xs Literal) => Expr xs Bool
e1 = equality (addition (multiplication (int 1) (negation (int 2))) (division (int 3) (int 4))) (multiplication (int 5) (int 2))

type Nodes = '[Comparison, Arithmetic, Literal]
type E = Expr Nodes
type Printers xs a = Printer (Expr xs) :* xs

printers :: (forall b. Expr Nodes b -> String) -> Printers Nodes a
printers p
    =  Printer (prettyComparison p)
    <: Printer (prettyArithmetic p)
    <: Printer (prettyLiteral)
    <: nil

pp :: Expr Nodes a -> String
pp = pretty (printers pp)

main :: IO ()
main = do
    putStrLn $ pp e1
    putStrLn "hello"
