{-# LANGUAGE OverloadedStrings, TypeOperators, GADTs, RankNTypes, KindSignatures #-}
module ExSql.Printer.Default
    ( Printer(..)
    , pretty
    , prettyLiteral
    , prettyArithmetic
    , prettyComparison
    , prettyLogical
    ) where

import Data.Extensible ((:*), (:|)(..), hindex)
import Data.Functor.Identity (Identity(..))
import Data.Text (Text)
import qualified Data.Text as Text (pack)
import ExSql.Syntax.Class (Expr(..), Node(..))
import ExSql.Syntax.Relativity (Relativity(..), Precedence(..), Associativity(..))
import ExSql.Syntax.Literal
import ExSql.Syntax.Arithmetic
import ExSql.Syntax.Comparison
import ExSql.Syntax.Logical

newtype Printer (g :: * -> *) (v :: (* -> *) -> * -> *) = Printer { runPrinter :: forall a. Maybe Relativity -> Maybe Relativity -> v g a -> Text }

pretty :: Printer (Expr xs Identity) :* xs -> Maybe Relativity -> Maybe Relativity -> Expr xs Identity a -> Text
pretty printers l r (Expr (EmbedAt membership (Node (Identity a)))) = runPrinter (hindex printers membership) l r a

prettyLiteral :: Maybe Relativity -> Maybe Relativity -> Literal g a -> Text
prettyLiteral _ _ (LitInt a) = Text.pack . show $ a
prettyLiteral _ _ (LitBool a) = Text.pack . show $ a

prettyComparison :: (forall b. Maybe Relativity -> Maybe Relativity -> Expr xs Identity b -> Text) -> Maybe Relativity -> Maybe Relativity -> Comparison (Expr xs Identity) a -> Text
prettyComparison p l r (Equality a0 a1) =
    let c = Relativity (Precedence 11) NonAssociative
    in handleBracket l c r $ p l (Just c) a0 `mappend` "==" `mappend` p (Just c) r a1
prettyComparison p l r (GreaterThan a0 a1) =
    let c = Relativity (Precedence 11) NonAssociative
    in handleBracket l c r $ p l (Just c) a0 `mappend` ">" `mappend` p (Just c) r a1
prettyComparison p l r (LessThan a0 a1) =
    let c = Relativity (Precedence 11) NonAssociative
    in handleBracket l c r $ p l (Just c) a0 `mappend` "<" `mappend` p (Just c) r a1
prettyComparison p l r (GreaterThanOrEqual a0 a1) =
    let c = Relativity (Precedence 11) NonAssociative
    in handleBracket l c r $ p l (Just c) a0 `mappend` ">=" `mappend` p (Just c) r a1
prettyComparison p l r (LessThanOrEqual a0 a1) =
    let c = Relativity (Precedence 11) NonAssociative
    in handleBracket l c r $ p l (Just c) a0 `mappend` "<=" `mappend` p (Just c) r a1

prettyArithmetic :: (forall b. Maybe Relativity -> Maybe Relativity -> Expr xs Identity b -> Text) -> Maybe Relativity -> Maybe Relativity -> Arithmetic (Expr xs Identity) a -> Text
prettyArithmetic p l r (Negation a) =
    let c = Relativity (Precedence 4) RightToLeft
    in handleBracket l c r $ "-" `mappend` p (Just c) r a
prettyArithmetic p l r (Addition a0 a1) =
    let c = Relativity (Precedence 7) LeftToRight
    in handleBracket l c r $ p l (Just c) a0 `mappend` "+" `mappend` p (Just c) r a1
prettyArithmetic p l r (Subtraction a0 a1) =
    let c = Relativity (Precedence 7) LeftToRight
    in handleBracket l c r $ p l (Just c) a0 `mappend` "-" `mappend` p (Just c) r a1
prettyArithmetic p l r (Multiplication a0 a1) =
    let c = Relativity (Precedence 6) LeftToRight
    in handleBracket l c r $ p l (Just c) a0 `mappend` "*" `mappend` p (Just c) r a1
prettyArithmetic p l r (Division a0 a1) =
    let c = Relativity (Precedence 6) LeftToRight
    in handleBracket l c r $ p l (Just c) a0 `mappend` "/" `mappend` p (Just c) r a1

prettyLogical :: (forall b. Maybe Relativity -> Maybe Relativity -> Expr xs Identity b -> Text) -> Maybe Relativity -> Maybe Relativity -> Logical (Expr xs Identity) a -> Text
prettyLogical p l r (LogicalNegation a) =
    let c = Relativity (Precedence 13) RightToLeft
    in handleBracket l c r $ "NOT " `mappend` p (Just c) r a
prettyLogical p l r (Conjunction a0 a1) =
    let c = Relativity (Precedence 14) LeftToRight
    in handleBracket l c r $ p l (Just c) a0 `mappend` " AND " `mappend` p (Just c) r a1
prettyLogical p l r (Disjunction a0 a1) =
    let c = Relativity (Precedence 16) LeftToRight
    in handleBracket l c r $ p l (Just c) a0 `mappend` " OR " `mappend` p (Just c) r a1

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
