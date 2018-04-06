{-# LANGUAGE DataKinds, GADTs, RankNTypes, FlexibleContexts #-}
module ExSql.Syntax.Logical
    ( Logical(..)
    , logicalNegation
    , conjunction
    , disjunction
    ) where

import Data.Proxy (Proxy(..))
import ExSql.Syntax.Class

data Logical g a where
    LogicalNegation :: g Bool -> Logical g Bool
    Conjunction :: g Bool -> g Bool -> Logical g Bool
    Disjunction :: g Bool -> g Bool -> Logical g Bool

type Uop g = UnaryOpType "Logical" Logical g Bool

type Bop g = BinaryOpType "Logical" Logical g Bool

proxy :: Proxy "Logical"
proxy = Proxy

logicalNegation :: Uop g
logicalNegation = unaryOp proxy LogicalNegation

conjunction :: Bop g
conjunction = binaryOp proxy Conjunction

disjunction :: Bop g
disjunction = binaryOp proxy Disjunction
