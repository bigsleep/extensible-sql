{-# LANGUAGE GADTs, RankNTypes, FlexibleContexts #-}
module ExSql.Syntax.Logical
    ( Logical(..)
    , logicalNegation
    , conjunction
    , disjunction
    ) where

import ExSql.Syntax.Class

data Logical g a where
    LogicalNegation :: g Bool -> Logical g Bool
    Conjunction :: g Bool -> g Bool -> Logical g Bool
    Disjunction :: g Bool -> g Bool -> Logical g Bool

instance Hoist Logical where
    hoist f (LogicalNegation a) = LogicalNegation (f a)
    hoist f (Conjunction a0 a1) = Conjunction (f a0) (f a1)
    hoist f (Disjunction a0 a1) = Disjunction (f a0) (f a1)

type Uop g = UnaryOpType Logical g Bool Bool

type Bop g = BinaryOpType Logical g Bool Bool

logicalNegation :: Uop g
logicalNegation = unaryOp LogicalNegation

conjunction :: Bop g
conjunction = binaryOp Conjunction

disjunction :: Bop g
disjunction = binaryOp Disjunction
