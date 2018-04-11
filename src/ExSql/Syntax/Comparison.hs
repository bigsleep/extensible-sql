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

instance Hoist Comparison where
    hoist f (Equality a0 a1) = Equality (f a0) (f a1)
    hoist f (GreaterThan a0 a1) = GreaterThan (f a0) (f a1)
    hoist f (LessThan a0 a1) = LessThan (f a0) (f a1)
    hoist f (GreaterThanOrEqual a0 a1) = GreaterThanOrEqual (f a0) (f a1)
    hoist f (LessThanOrEqual a0 a1) = LessThanOrEqual (f a0) (f a1)

type Bop g m a = BinaryOpType Comparison g m a Bool

equality :: Bop g m a
equality = binaryOp Equality

greaterThan :: Bop g m a
greaterThan = binaryOp GreaterThan

lessThan :: Bop g m a
lessThan = binaryOp LessThan

greaterThanOrEqual :: Bop g m a
greaterThanOrEqual = binaryOp GreaterThanOrEqual

lessThanOrEqual :: Bop g m a
lessThanOrEqual = binaryOp GreaterThanOrEqual
