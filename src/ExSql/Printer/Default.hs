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
import qualified Data.Text as Text (pack, intercalate)

import Database.Persist (PersistValue(..))

import ExSql.Syntax.Class (Expr(..), Node(..))
import ExSql.Syntax.Relativity (Relativity(..), Precedence(..), Associativity(..))
import ExSql.Syntax.Arithmetic
import ExSql.Syntax.Comparison
import ExSql.Syntax.Function
import ExSql.Syntax.Literal
import ExSql.Syntax.Logical

newtype Printer (g :: * -> *) (v :: (* -> *) -> * -> *) = Printer { runPrinter :: forall a. Maybe Relativity -> Maybe Relativity -> v g a -> (Text, DList PersistValue) }

pretty :: Printer (Expr xs Identity) :* xs -> Maybe Relativity -> Maybe Relativity -> Expr xs Identity a -> (Text, DList PersistValue)
pretty printers l r (Expr (EmbedAt membership (Node (Identity a)))) = runPrinter (hindex printers membership) l r a

prettyBinOp :: (forall x. Maybe Relativity -> Maybe Relativity -> Expr xs Identity x -> (Text, DList PersistValue)) -> Maybe Relativity -> Relativity -> Maybe Relativity -> Text -> Expr xs Identity a -> Expr xs Identity b -> (Text, DList PersistValue)
prettyBinOp p l c r op a b =
    let (lt, lps) = p l (Just c) a
        (rt, rps) = p (Just c) r b
    in (handleBracket l c r $ lt `mappend` op `mappend` rt, lps `mappend` rps)

prettyFun :: Text -> [(Text, DList PersistValue)] -> (Text, DList PersistValue)
prettyFun fname args = (fname `mappend` "(" `mappend` Text.intercalate ", " xs `mappend` ")", ps)
    where
    xs = map fst args
    ps = mconcat $ map snd args

prettyLiteral :: (forall b. Maybe Relativity -> Maybe Relativity -> Expr xs Identity b -> (Text, DList PersistValue)) -> Maybe Relativity -> Maybe Relativity -> Literal (Expr xs Identity) a -> (Text, DList PersistValue)
prettyLiteral _ _ _ (LitInt a) = ("?", return $ PersistInt64 a)
prettyLiteral _ _ _ (LitBool a) = ("?", return $ PersistBool a)
prettyLiteral p _ _ (LitValueList a) = ("(" `mappend` x `mappend` ")", ps)
    where
    elems = map (p Nothing Nothing) a
    x = Text.intercalate ", " $ map fst elems
    ps = mconcat $ map snd elems

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

prettyFunction :: (forall b. Maybe Relativity -> Maybe Relativity -> Expr xs Identity b -> (Text, DList PersistValue)) -> Maybe Relativity -> Maybe Relativity -> Function (Expr xs Identity) a -> (Text, DList PersistValue)
prettyFunction _ _ _ (Function0 fname) = prettyFun fname []
prettyFunction p _ _ (Function1 fname a0) =
    prettyFun fname $ [p Nothing Nothing a0]
prettyFunction p _ _ (Function2 fname a0 a1) =
    prettyFun fname $ [p Nothing Nothing a0, p Nothing Nothing a1]
prettyFunction p _ _ (Function3 fname a0 a1 a2) =
    prettyFun fname $ [p Nothing Nothing a0, p Nothing Nothing a1, p Nothing Nothing a2]
prettyFunction p _ _ (Function4 fname a0 a1 a2 a3) =
    prettyFun fname $ [p Nothing Nothing a0, p Nothing Nothing a1, p Nothing Nothing a2, p Nothing Nothing a3]
prettyFunction p _ _ (Function5 fname a0 a1 a2 a3 a4) =
    prettyFun fname $ [p Nothing Nothing a0, p Nothing Nothing a1, p Nothing Nothing a2, p Nothing Nothing a3, p Nothing Nothing a4]
prettyFunction p _ _ (Function6 fname a0 a1 a2 a3 a4 a5) =
    prettyFun fname $ [p Nothing Nothing a0, p Nothing Nothing a1, p Nothing Nothing a2, p Nothing Nothing a3, p Nothing Nothing a4, p Nothing Nothing a5]

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
