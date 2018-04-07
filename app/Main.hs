{-# LANGUAGE DataKinds, FlexibleContexts, TypeOperators #-}
module Main where

import Data.Extensible (Member, Match(..), (:*), (<:), nil)
import ExSql.Syntax.Arithmetic
import ExSql.Syntax.Literal
import ExSql.Syntax.Class
import ExSql.Printer.Default

e1 :: (Member xs Arithmetic, Member xs Literal) => Expr xs Int
e1 = addition (multiplication (int 1) (negation (int 2))) (division (int 3) (int 4))

type Nodes = '[Arithmetic, Literal]
type E = Expr Nodes
type Printers xs a = Match (Node (Expr xs) a) String :* xs

printers :: (Expr Nodes a -> String) -> Printers Nodes a
printers p
    = Match (prettyArithmetic p . unNode)
    <: Match (prettyLiteral . unNode)
    <: nil

pp :: Expr Nodes Int -> String
pp = pretty (printers pp)

main :: IO ()
main = do
    putStrLn $ pp e1
    putStrLn "hello"
