{-# LANGUAGE DataKinds, FlexibleContexts, TypeOperators, RankNTypes #-}
module Main where

import Data.Text (Text)
import qualified Data.Text as Text (unpack)
import Data.Extensible (Member, Match(..), (:*), (<:), nil)
import Data.Functor.Identity (Identity(..))
import ExSql.Syntax.Arithmetic
import ExSql.Syntax.Comparison
import ExSql.Syntax.Logical
import ExSql.Syntax.Literal
import ExSql.Syntax.Class
import ExSql.Syntax.Relativity
import ExSql.Printer.Default

e1 :: (Member xs Comparison, Member xs Arithmetic, Member xs Literal, Member xs Logical, Monad m) => Expr m xs Bool
e1 = disjunction (bool False) (conjunction (equality (addition (multiplication (int 1) (negation (int 2))) (division (int 3) (addition (int 4) (int 2)))) (multiplication (int 5) (int 2))) (bool True))

type Nodes = '[Logical, Comparison, Arithmetic, Literal]
type E = Expr Identity Nodes
type Printers xs a = Printer (Expr Identity xs) :* xs

printers :: (forall b. Maybe Relativity -> Maybe Relativity -> Expr Identity Nodes b -> Text) -> Printers Nodes a
printers p
    =  Printer (prettyLogical p)
    <: Printer (prettyComparison p)
    <: Printer (prettyArithmetic p)
    <: Printer (prettyLiteral)
    <: nil

pp :: Maybe Relativity -> Maybe Relativity -> Expr Identity Nodes a -> Text
pp = pretty (printers pp)

main :: IO ()
main = do
    putStrLn . Text.unpack $ pp Nothing Nothing e1
    putStrLn "hello"
