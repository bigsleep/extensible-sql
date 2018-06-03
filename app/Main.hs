{-# LANGUAGE OverloadedStrings, DataKinds, FlexibleContexts, GADTs, GeneralizedNewtypeDeriving, KindSignatures, MultiParamTypeClasses, RankNTypes, TypeFamilies, TypeOperators, QuasiQuotes, TemplateHaskell #-}
module Main where

import qualified Control.Monad.Trans.State.Strict as State (runStateT)
import Data.DList (DList)
import Data.Text (Text)
import qualified Data.Text as Text (unpack)
import qualified Data.Text.Lazy.Builder as TLB
import Data.Extensible (Member, Match(..), (:*), (<:), nil)
import Data.Functor.Identity (Identity(..))
import Database.Persist (Entity, PersistValue(..))
import qualified Database.Persist.TH as Persist (mkPersist, persistLowerCase, share, sqlSettings)
import ExSql.Syntax.Arithmetic
import ExSql.Syntax.Comparison
import ExSql.Syntax.Logical
import ExSql.Syntax.Literal
import ExSql.Syntax.Class
import ExSql.Syntax.Relativity
import ExSql.Syntax.SelectQuery
import ExSql.Printer.Default
import ExSql.Printer.SelectQuery
import ExSql.Printer.Types

Persist.share [Persist.mkPersist Persist.sqlSettings] [Persist.persistLowerCase|
Person
    name String
    age Int
    deriving Show
|]

e1 :: (Member xs Comparison, Member xs Arithmetic, Member xs Literal, Member xs Logical, Monad m) => Expr xs m Bool
e1 = disjunction (bool False) (conjunction (equality (addition (multiplication (int 1) (negation (int 2))) (division (int 3) (addition (int 4) (int 2)))) (multiplication (int 5) (int 2))) (bool True))

type Nodes = '[Logical, Comparison, Arithmetic, Literal]
type E = Expr Nodes Identity
type Printers xs a = Printer (Expr xs Identity) :* xs

printers :: ExprPrinterType (Expr Nodes Identity) -> Printers Nodes a
printers p
    =  Printer (printLogical p)
    <: Printer (printComparison p)
    <: Printer (printArithmetic p)
    <: Printer (printLiteral p)
    <: nil

pp :: Maybe Relativity -> Maybe Relativity -> Expr Nodes Identity a -> StatementBuilder
pp = printExpr (printers pp)

s1 :: SelectQuery E (Entity Person)
s1 = selectFrom $ \person -> where_ e1

main :: IO ()
main = do
    let (c, s) = renderSelect pp s1
        r = State.runStateT c [PersistInt64 1, PersistText "abc", PersistInt64 20]
    print s
    print r
    print $ printSelect pp s1
    putStrLn "hello"
