{-# LANGUAGE DataKinds, GADTs, RankNTypes, FlexibleContexts #-}
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

type Uop g = UnaryOpType Logical g Bool Bool

type Bop g = BinaryOpType Logical g Bool Bool

logicalNegation :: Uop g
logicalNegation = unaryOp LogicalNegation

conjunction :: Bop g
conjunction = binaryOp Conjunction

disjunction :: Bop g
disjunction = binaryOp Disjunction
