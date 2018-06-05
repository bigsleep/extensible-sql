{-# LANGUAGE
    FlexibleContexts,
    GADTs,
    RankNTypes
#-}
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

type Uop g m = UnaryOpType Logical g m Bool Bool

type Bop g m = BinaryOpType Logical g m Bool Bool

logicalNegation :: Uop g m
logicalNegation = unaryOp LogicalNegation

conjunction :: Bop g m
conjunction = binaryOp Conjunction

disjunction :: Bop g m
disjunction = binaryOp Disjunction
