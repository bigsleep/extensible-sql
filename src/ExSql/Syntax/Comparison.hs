{-# LANGUAGE GADTs, RankNTypes, FlexibleContexts #-}
module ExSql.Syntax.Comparison
    ( Comparison(..)
    , equality
    , greaterThan
    , lessThan
    , greaterThanOrEqual
    , lessThanOrEqual
    ) where

import ExSql.Syntax.Class

data Comparison g a where
    Equality :: g a -> g a -> Comparison g Bool
    GreaterThan :: g a -> g a -> Comparison g Bool
    LessThan :: g a -> g a -> Comparison g Bool
    GreaterThanOrEqual :: g a -> g a -> Comparison g Bool
    LessThanOrEqual :: g a -> g a -> Comparison g Bool

type Bop g a = BinaryOpType Comparison g a Bool

equality :: Bop g a
equality = binaryOp Equality

greaterThan :: Bop g a
greaterThan = binaryOp GreaterThan

lessThan :: Bop g a
lessThan = binaryOp LessThan

greaterThanOrEqual :: Bop g a
greaterThanOrEqual = binaryOp GreaterThanOrEqual

lessThanOrEqual :: Bop g a
lessThanOrEqual = binaryOp GreaterThanOrEqual

