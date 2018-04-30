{-# LANGUAGE OverloadedStrings, TypeOperators, GADTs, RankNTypes, KindSignatures #-}
module ExSql.Printer.Default
    ( Printer(..)
    , pretty
    , prettyLiteral
    , prettyArithmetic
    , prettyComparison
    , prettyLogical
    ) where

import Data.DList (DList)
import qualified Data.DList as DList
import Data.Extensible ((:*), (:|)(..), hindex)
import Data.Functor.Identity (Identity(..))
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as Text (pack)

import Database.Persist (PersistValue(..))

import ExSql.Syntax.Class (Expr(..), Node(..))
import ExSql.Syntax.Relativity (Relativity(..), Precedence(..), Associativity(..))
import ExSql.Syntax.Literal
import ExSql.Syntax.Arithmetic
import ExSql.Syntax.Comparison
import ExSql.Syntax.Logical

newtype Printer (g :: * -> *) (v :: (* -> *) -> * -> *) = Printer { runPrinter :: forall a. Maybe Relativity -> Maybe Relativity -> v g a -> (Text, DList PersistValue) }

pretty :: Printer (Expr xs Identity) :* xs -> Maybe Relativity -> Maybe Relativity -> Expr xs Identity a -> (Text, DList PersistValue)
pretty printers l r (Expr (EmbedAt membership (Node (Identity a)))) = runPrinter (hindex printers membership) l r a

prettyBinOp :: (forall x. Maybe Relativity -> Maybe Relativity -> Expr xs Identity x -> (Text, DList PersistValue)) -> Maybe Relativity -> Relativity -> Maybe Relativity -> Text -> Expr xs Identity a -> Expr xs Identity b -> (Text, DList PersistValue)
prettyBinOp p l c r op a b =
    let (lt, lps) = p l (Just c) a
        (rt, rps) = p (Just c) r b
    in (handleBracket l c r $ lt `mappend` op `mappend` rt, lps `mappend` rps)

prettyLiteral :: Maybe Relativity -> Maybe Relativity -> Literal g a -> (Text, DList PersistValue)
prettyLiteral _ _ (LitInt a) = ("?", return $ PersistInt64 a)
prettyLiteral _ _ (LitBool a) = ("?", return $ PersistBool a)

prettyComparison :: (forall b. Maybe Relativity -> Maybe Relativity -> Expr xs Identity b -> (Text, DList PersistValue)) -> Maybe Relativity -> Maybe Relativity -> Comparison (Expr xs Identity) a -> (Text, DList PersistValue)
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

prettyArithmetic :: (forall b. Maybe Relativity -> Maybe Relativity -> Expr xs Identity b -> (Text, DList PersistValue)) -> Maybe Relativity -> Maybe Relativity -> Arithmetic (Expr xs Identity) a -> (Text, DList PersistValue)
prettyArithmetic p l r (Negation a) =
    let c = Relativity (Precedence 4) RightToLeft
        (t, ps) = p (Just c) r a
    in (handleBracket l c r $ "-" `mappend` t, ps)
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

prettyLogical :: (forall b. Maybe Relativity -> Maybe Relativity -> Expr xs Identity b -> (Text, DList PersistValue)) -> Maybe Relativity -> Maybe Relativity -> Logical (Expr xs Identity) a -> (Text, DList PersistValue)
prettyLogical p l r (LogicalNegation a) =
    let c = Relativity (Precedence 13) RightToLeft
        (t, ps) = p (Just c) r a
    in (handleBracket l c r $ "NOT " `mappend` t, ps)
prettyLogical p l r (Conjunction a0 a1) =
    let c = Relativity (Precedence 14) LeftToRight
    in prettyBinOp p l c r " AND " a0 a1
prettyLogical p l r (Disjunction a0 a1) =
    let c = Relativity (Precedence 16) LeftToRight
    in prettyBinOp p l c r " OR " a0 a1

handleBracket :: Maybe Relativity -> Relativity -> Maybe Relativity -> Text -> Text
handleBracket l c r s = if needBracket l c r
    then "(" `mappend` s `mappend` ")"
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
