{-# LANGUAGE OverloadedStrings, TypeOperators, GADTs, RankNTypes, KindSignatures #-}
module ExSql.Printer.Default
    ( Printer(..)
    , pretty
    , prettyLiteral
    , prettyArithmetic
    , prettyColumn
    , prettyComparison
    , prettyFunction
    , prettyIn
    , prettyLogical
    ) where

import Data.DList (DList)
import qualified Data.DList as DList
import Data.Extensible ((:*), (:|)(..), hindex)
import Data.Functor.Identity (Identity(..))
import Data.Int (Int64)
import qualified Data.List as List (intersperse)
import Data.Text (Text)
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Text.Lazy.Builder.Int as TLB

import Database.Persist (PersistValue(..))
import qualified Database.Persist as Persist (DBName(..))
import qualified Database.Persist.Sql as Persist (fieldDBName)

import ExSql.Printer.Types (Printer(..), PrinterType, ExprPrinterType, StatementBuilder(..))
import ExSql.Syntax.Class (Expr(..), Node(..))
import ExSql.Syntax.Relativity (Relativity(..), Precedence(..), Associativity(..))
import ExSql.Syntax.Arithmetic
import ExSql.Syntax.Column
import ExSql.Syntax.Comparison
import ExSql.Syntax.Function
import ExSql.Syntax.In
import ExSql.Syntax.Literal
import ExSql.Syntax.Logical
import ExSql.Syntax.Internal.Row
import ExSql.Syntax.Internal.Types

pretty :: Printer (Expr xs Identity) :* xs -> Maybe Relativity -> Maybe Relativity -> Expr xs Identity a -> StatementBuilder
pretty printers l r (Expr (EmbedAt membership (Node (Identity a)))) = runPrinter (hindex printers membership) l r a

prettyBinOp :: ExprPrinterType (Expr xs Identity) -> Maybe Relativity -> Relativity -> Maybe Relativity -> Text -> Expr xs Identity a -> Expr xs Identity b -> StatementBuilder
prettyBinOp p l c r op a b =
    let StatementBuilder (lt, lps) = p l (Just c) a
        StatementBuilder (rt, rps) = p (Just c) r b
    in StatementBuilder (handleBracket l c r $ lt `mappend` TLB.fromText op `mappend` rt, lps `mappend` rps)

prettyVals :: [StatementBuilder] -> StatementBuilder
prettyVals vals = StatementBuilder (t, ps)
    where
    xs = map (fst . unStatementBuilder) vals
    t = TLB.singleton '('
        `mappend` mconcat (List.intersperse (TLB.fromText ", ") xs)
        `mappend` TLB.singleton ')'
    ps = mconcat $ map (snd . unStatementBuilder) vals

prettyFun :: Text -> [StatementBuilder] -> StatementBuilder
prettyFun fname args = StatementBuilder (t, ps)
    where
    StatementBuilder (x, ps) = prettyVals args
    t = TLB.fromText fname `mappend` x

prettyRow :: ExprPrinterType (Expr xs Identity) -> Maybe Relativity -> Maybe Relativity -> Row (Expr xs Identity) a -> StatementBuilder
prettyRow p l r (Row a) = p l r a
prettyRow p _ _ (Row2 (a0, a1)) = prettyVals [p Nothing Nothing a0, p Nothing Nothing a1]
prettyRow p _ _ (Row3 (a0, a1, a2)) = prettyVals [p Nothing Nothing a0, p Nothing Nothing a1, p Nothing Nothing a2]
prettyRow p _ _ (Row4 (a0, a1, a2, a3)) = prettyVals [p Nothing Nothing a0, p Nothing Nothing a1, p Nothing Nothing a2, p Nothing Nothing a3]
prettyRow p _ _ (Row5 (a0, a1, a2, a3, a4)) = prettyVals [p Nothing Nothing a0, p Nothing Nothing a1, p Nothing Nothing a2, p Nothing Nothing a3, p Nothing Nothing a4]
prettyRow p _ _ (Row6 (a0, a1, a2, a3, a4, a5)) = prettyVals [p Nothing Nothing a0, p Nothing Nothing a1, p Nothing Nothing a2, p Nothing Nothing a3, p Nothing Nothing a4, p Nothing Nothing a5]

prettyLiteral :: ExprPrinterType (Expr xs Identity) -> PrinterType (Expr xs Identity) Literal a
prettyLiteral _ _ _ (LitInt a) = StatementBuilder (TLB.singleton '?', return $ PersistInt64 a)
prettyLiteral _ _ _ (LitBool a) = StatementBuilder (TLB.singleton '?', return $ PersistBool a)
prettyLiteral p _ _ (LitValueList a) = StatementBuilder (t, ps)
    where
    elems = map (p Nothing Nothing) a
    xs = map (fst . unStatementBuilder) elems
    t = TLB.singleton '('
        `mappend` mconcat (List.intersperse (TLB.fromText ", ") xs)
        `mappend` TLB.singleton ')'
    ps = mconcat $ map (snd . unStatementBuilder) elems

prettyComparison :: ExprPrinterType (Expr xs Identity) -> PrinterType (Expr xs Identity) Comparison a
prettyComparison p l r (Equality a0 a1) =
    let c = Relativity (Precedence 11) NonAssociative
    in prettyBinOp p l c r "==" a0 a1
prettyComparison p l r (GreaterThan a0 a1) =
    let c = Relativity (Precedence 11) NonAssociative
    in prettyBinOp p l c r ">" a0 a1
prettyComparison p l r (LessThan a0 a1) =
    let c = Relativity (Precedence 11) NonAssociative
    in prettyBinOp p l c r "<" a0 a1
prettyComparison p l r (GreaterThanOrEqual a0 a1) =
    let c = Relativity (Precedence 11) NonAssociative
    in prettyBinOp p l c r ">=" a0 a1
prettyComparison p l r (LessThanOrEqual a0 a1) =
    let c = Relativity (Precedence 11) NonAssociative
    in prettyBinOp p l c r "<=" a0 a1

prettyArithmetic :: ExprPrinterType (Expr xs Identity) -> PrinterType (Expr xs Identity) Arithmetic a
prettyArithmetic p l r (Negation a) =
    let c = Relativity (Precedence 4) RightToLeft
        StatementBuilder (t, ps) = p (Just c) r a
    in StatementBuilder (handleBracket l c r $ TLB.singleton '-' `mappend` t, ps)
prettyArithmetic p l r (Addition a0 a1) =
    let c = Relativity (Precedence 7) LeftToRight
    in prettyBinOp p l c r "+" a0 a1
prettyArithmetic p l r (Subtraction a0 a1) =
    let c = Relativity (Precedence 7) LeftToRight
    in prettyBinOp p l c r "-" a0 a1
prettyArithmetic p l r (Multiplication a0 a1) =
    let c = Relativity (Precedence 6) LeftToRight
    in prettyBinOp p l c r "*" a0 a1
prettyArithmetic p l r (Division a0 a1) =
    let c = Relativity (Precedence 6) LeftToRight
    in prettyBinOp p l c r "/" a0 a1

prettyIn :: ExprPrinterType (Expr xs Identity) -> PrinterType (Expr xs Identity) In a
prettyIn p l r (In a b) =
    let c = Just $ Relativity (Precedence 12) NonAssociative
        StatementBuilder (t0, ps0) = prettyRow p l c a
        StatementBuilder (t1, ps1) = p c r b
        t = t0 `mappend` TLB.fromText " IN " `mappend` t1
        ps = ps0 `mappend` ps1
    in StatementBuilder (t, ps)

prettyLogical :: ExprPrinterType (Expr xs Identity) -> PrinterType (Expr xs Identity) Logical a
prettyLogical p l r (LogicalNegation a) =
    let c = Relativity (Precedence 13) RightToLeft
        StatementBuilder (t, ps) = p (Just c) r a
    in StatementBuilder (handleBracket l c r $ TLB.fromText "NOT " `mappend` t, ps)
prettyLogical p l r (Conjunction a0 a1) =
    let c = Relativity (Precedence 14) LeftToRight
    in prettyBinOp p l c r " AND " a0 a1
prettyLogical p l r (Disjunction a0 a1) =
    let c = Relativity (Precedence 16) LeftToRight
    in prettyBinOp p l c r " OR " a0 a1

prettyFunction :: ExprPrinterType (Expr xs Identity) -> PrinterType (Expr xs Identity) Function a
prettyFunction _ _ _ (Function0 fname) = prettyFun fname []
prettyFunction p _ _ (Function1 fname a0) =
    prettyFun fname [p Nothing Nothing a0]
prettyFunction p _ _ (Function2 fname a0 a1) =
    prettyFun fname [p Nothing Nothing a0, p Nothing Nothing a1]
prettyFunction p _ _ (Function3 fname a0 a1 a2) =
    prettyFun fname [p Nothing Nothing a0, p Nothing Nothing a1, p Nothing Nothing a2]
prettyFunction p _ _ (Function4 fname a0 a1 a2 a3) =
    prettyFun fname [p Nothing Nothing a0, p Nothing Nothing a1, p Nothing Nothing a2, p Nothing Nothing a3]
prettyFunction p _ _ (Function5 fname a0 a1 a2 a3 a4) =
    prettyFun fname [p Nothing Nothing a0, p Nothing Nothing a1, p Nothing Nothing a2, p Nothing Nothing a3, p Nothing Nothing a4]
prettyFunction p _ _ (Function6 fname a0 a1 a2 a3 a4 a5) =
    prettyFun fname [p Nothing Nothing a0, p Nothing Nothing a1, p Nothing Nothing a2, p Nothing Nothing a3, p Nothing Nothing a4, p Nothing Nothing a5]

prettyColumn :: ExprPrinterType (Expr xs Identity) -> PrinterType (Expr xs Identity) Column a
prettyColumn p l r (Column (Ref tid) col) =
    let c = Relativity (Precedence 3) LeftToRight
        prefix = "t_"
        columnName = Persist.unDBName . Persist.fieldDBName $ col
        x = TLB.singleton '`'
            `mappend` TLB.fromText prefix
            `mappend` TLB.decimal tid
            `mappend` TLB.singleton '`'
            `mappend` TLB.singleton '.'
            `mappend` TLB.singleton '`'
            `mappend` TLB.fromText columnName
            `mappend` TLB.singleton '`'
    in StatementBuilder (handleBracket l c r x, mempty)

addBracket :: TLB.Builder -> TLB.Builder
addBracket a = TLB.singleton '('
    `mappend` a
    `mappend` TLB.singleton ')'

handleBracket :: Maybe Relativity -> Relativity -> Maybe Relativity -> TLB.Builder -> TLB.Builder
handleBracket l c r s = if needBracket l c r
    then addBracket s
    else s

needBracket :: Maybe Relativity -> Relativity -> Maybe Relativity -> Bool
needBracket l c r = needBracketL c r || needBracketR l c

needBracketL :: Relativity -> Maybe Relativity -> Bool
needBracketL (Relativity p a) (Just (Relativity rp ra))
    | p > rp = True
    | a == NonAssociative = True
    | p == rp && a == LeftToRight = True
    | otherwise = False
needBracketL _ _ = False

needBracketR :: Maybe Relativity -> Relativity -> Bool
needBracketR (Just (Relativity lp la)) (Relativity p a)
    | p > lp = True
    | a == NonAssociative = True
    | p == lp && a == RightToLeft = True
    | otherwise = False
needBracketR _ _ = False
