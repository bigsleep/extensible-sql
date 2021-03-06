{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeOperators     #-}
module ExSql.Printer.Default
    ( Printer(..)
    , printExpr
    , printLiteral
    , printArithmetic
    , printSubSelect
    , printComparison
    , printFunction
    , printIn
    , printLogical
    ) where

import Data.Extensible ((:*), (:|)(..), hindex)
import Data.Functor.Identity (Identity(..))
import qualified Data.List as List (intersperse)
import qualified Data.List.NonEmpty as NonEmpty (toList)
import Data.Text (Text)
import qualified Data.Text.Lazy.Builder as TLB

import Database.Persist (PersistValue(..))

import ExSql.Printer.Common
import ExSql.Printer.SelectQuery
import ExSql.Printer.Types (ExprPrinterType, Printer(..), PrinterType,
                            StatementBuilder(..))
import ExSql.Syntax.Arithmetic
import ExSql.Syntax.Class (Expr(..), Node(..))
import ExSql.Syntax.Comparison
import ExSql.Syntax.Function
import ExSql.Syntax.In
import ExSql.Syntax.Internal.Row
import ExSql.Syntax.Literal
import ExSql.Syntax.Logical
import ExSql.Syntax.Relativity (Associativity(..), Precedence(..),
                                Relativity(..))
import ExSql.Syntax.SubSelect

printExpr :: Printer (Expr xs Identity) :* xs -> Maybe Relativity -> Maybe Relativity -> Expr xs Identity a -> StatementBuilder
printExpr printers l r (Expr (EmbedAt membership (Node (Identity a)))) = runPrinter (hindex printers membership) l r a

printBinOp :: ExprPrinterType (Expr xs Identity) -> Maybe Relativity -> Relativity -> Maybe Relativity -> Text -> Expr xs Identity a -> Expr xs Identity b -> StatementBuilder
printBinOp p l c r op a b =
    let StatementBuilder (lt, lps) = p l (Just c) a
        StatementBuilder (rt, rps) = p (Just c) r b
    in StatementBuilder (handleBracket l c r $ lt `mappend` TLB.fromText op `mappend` rt, lps `mappend` rps)

printRow :: ExprPrinterType (Expr xs Identity) -> Maybe Relativity -> Maybe Relativity -> Row (Expr xs Identity) a -> StatementBuilder
printRow p l r (Row a) = p l r a
printRow p _ _ (Row2 (a0, a1)) = printVals [p Nothing Nothing a0, p Nothing Nothing a1]
printRow p _ _ (Row3 (a0, a1, a2)) = printVals [p Nothing Nothing a0, p Nothing Nothing a1, p Nothing Nothing a2]
printRow p _ _ (Row4 (a0, a1, a2, a3)) = printVals [p Nothing Nothing a0, p Nothing Nothing a1, p Nothing Nothing a2, p Nothing Nothing a3]
printRow p _ _ (Row5 (a0, a1, a2, a3, a4)) = printVals [p Nothing Nothing a0, p Nothing Nothing a1, p Nothing Nothing a2, p Nothing Nothing a3, p Nothing Nothing a4]
printRow p _ _ (Row6 (a0, a1, a2, a3, a4, a5)) = printVals [p Nothing Nothing a0, p Nothing Nothing a1, p Nothing Nothing a2, p Nothing Nothing a3, p Nothing Nothing a4, p Nothing Nothing a5]

printLiteral :: ExprPrinterType (Expr xs Identity) -> PrinterType (Expr xs Identity) Literal a
printLiteral _ _ _ (LitInt a) = StatementBuilder (TLB.singleton '?', return $ PersistInt64 a)
printLiteral _ _ _ (LitBool a) = StatementBuilder (TLB.singleton '?', return $ PersistBool a)
printLiteral _ _ _ (LitText a) = StatementBuilder (TLB.singleton '?', return $ PersistText a)
printLiteral p _ _ (LitValueList a) = StatementBuilder (t, ps)
    where
    elems = map (p Nothing Nothing) (NonEmpty.toList a)
    xs = map (fst . unStatementBuilder) elems
    t = TLB.singleton '('
        `mappend` mconcat (List.intersperse (TLB.fromText ", ") xs)
        `mappend` TLB.singleton ')'
    ps = mconcat $ map (snd . unStatementBuilder) elems

printComparison :: ExprPrinterType (Expr xs Identity) -> PrinterType (Expr xs Identity) Comparison a
printComparison p l r (Equality a0 a1) =
    let c = Relativity (Precedence 11) NonAssociative
    in printBinOp p l c r "==" a0 a1
printComparison p l r (GreaterThan a0 a1) =
    let c = Relativity (Precedence 11) NonAssociative
    in printBinOp p l c r ">" a0 a1
printComparison p l r (LessThan a0 a1) =
    let c = Relativity (Precedence 11) NonAssociative
    in printBinOp p l c r "<" a0 a1
printComparison p l r (GreaterThanOrEqual a0 a1) =
    let c = Relativity (Precedence 11) NonAssociative
    in printBinOp p l c r ">=" a0 a1
printComparison p l r (LessThanOrEqual a0 a1) =
    let c = Relativity (Precedence 11) NonAssociative
    in printBinOp p l c r "<=" a0 a1

printArithmetic :: ExprPrinterType (Expr xs Identity) -> PrinterType (Expr xs Identity) Arithmetic a
printArithmetic p l r (Negation a) =
    let c = Relativity (Precedence 4) RightToLeft
        StatementBuilder (t, ps) = p (Just c) r a
    in StatementBuilder (handleBracket l c r $ TLB.singleton '-' `mappend` t, ps)
printArithmetic p l r (Addition a0 a1) =
    let c = Relativity (Precedence 7) LeftToRight
    in printBinOp p l c r "+" a0 a1
printArithmetic p l r (Subtraction a0 a1) =
    let c = Relativity (Precedence 7) LeftToRight
    in printBinOp p l c r "-" a0 a1
printArithmetic p l r (Multiplication a0 a1) =
    let c = Relativity (Precedence 6) LeftToRight
    in printBinOp p l c r "*" a0 a1
printArithmetic p l r (Division a0 a1) =
    let c = Relativity (Precedence 6) LeftToRight
    in printBinOp p l c r "/" a0 a1

printIn :: ExprPrinterType (Expr xs Identity) -> PrinterType (Expr xs Identity) In a
printIn p l r (In a b) =
    let c = Just $ Relativity (Precedence 12) NonAssociative
        StatementBuilder (t0, ps0) = printRow p l c a
        StatementBuilder (t1, ps1) = p c r b
        t = t0 `mappend` TLB.fromText " IN " `mappend` t1
        ps = ps0 `mappend` ps1
    in StatementBuilder (t, ps)

printLogical :: ExprPrinterType (Expr xs Identity) -> PrinterType (Expr xs Identity) Logical a
printLogical p l r (LogicalNegation a) =
    let c = Relativity (Precedence 13) RightToLeft
        StatementBuilder (t, ps) = p (Just c) r a
    in StatementBuilder (handleBracket l c r $ TLB.fromText "NOT " `mappend` t, ps)
printLogical p l r (Conjunction a0 a1) =
    let c = Relativity (Precedence 14) LeftToRight
    in printBinOp p l c r " AND " a0 a1
printLogical p l r (Disjunction a0 a1) =
    let c = Relativity (Precedence 16) LeftToRight
    in printBinOp p l c r " OR " a0 a1

printFunction :: ExprPrinterType (Expr xs Identity) -> PrinterType (Expr xs Identity) Function a
printFunction _ _ _ (Function0 fname) = printFun fname []
printFunction p _ _ (Function1 fname a0) =
    printFun fname [p Nothing Nothing a0]
printFunction p _ _ (Function2 fname a0 a1) =
    printFun fname [p Nothing Nothing a0, p Nothing Nothing a1]
printFunction p _ _ (Function3 fname a0 a1 a2) =
    printFun fname [p Nothing Nothing a0, p Nothing Nothing a1, p Nothing Nothing a2]
printFunction p _ _ (Function4 fname a0 a1 a2 a3) =
    printFun fname [p Nothing Nothing a0, p Nothing Nothing a1, p Nothing Nothing a2, p Nothing Nothing a3]
printFunction p _ _ (Function5 fname a0 a1 a2 a3 a4) =
    printFun fname [p Nothing Nothing a0, p Nothing Nothing a1, p Nothing Nothing a2, p Nothing Nothing a3, p Nothing Nothing a4]
printFunction p _ _ (Function6 fname a0 a1 a2 a3 a4 a5) =
    printFun fname [p Nothing Nothing a0, p Nothing Nothing a1, p Nothing Nothing a2, p Nothing Nothing a3, p Nothing Nothing a4, p Nothing Nothing a5]

printSubSelect :: ExprPrinterType (Expr xs Identity) -> PrinterType (Expr xs Identity) SubSelect a
printSubSelect p _ _ (SubSelect query) =
    let StatementBuilder (t, ps) = printSelect p query
    in StatementBuilder (addBracket t, ps)
printSubSelect p _ _ (SubSelectValues query) =
    let StatementBuilder (t, ps) = printSelect p query
    in StatementBuilder (addBracket t, ps)
