{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
module Main where

import qualified Control.Monad.Trans.State.Strict as State (runStateT)
import Data.Extensible ((:*), Member, nil, (<:))
import Data.Functor.Identity (Identity(..))
import Database.Persist (Entity, PersistValue(..))
import qualified Database.Persist.TH as Persist (mkPersist, persistLowerCase,
                                                 share, sqlSettings)
import ExSql.Printer.Default
import ExSql.Printer.SelectQuery
import ExSql.Printer.Types
import ExSql.Syntax.Arithmetic
import ExSql.Syntax.Class
import ExSql.Syntax.Comparison
import ExSql.Syntax.Literal
import ExSql.Syntax.Logical
import ExSql.Syntax.Relativity
import ExSql.Syntax.SelectQuery

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

s1 :: SelectQuery FieldsSelector E (Entity Person)
s1 = select_ . fromEntity $ \_ _ -> where_ e1

main :: IO ()
main = do
    let (c, s) = renderSelect pp s1
        r = State.runStateT c [PersistInt64 1, PersistText "abc", PersistInt64 20]
    print s
    print r
    print $ printSelect pp s1
    putStrLn "hello"
