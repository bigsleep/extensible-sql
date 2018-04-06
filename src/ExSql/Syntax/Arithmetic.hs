{-# LANGUAGE DataKinds, GADTs, RankNTypes, FlexibleContexts #-}
module ExSql.Syntax.Arithmetic
    ( Arithmetic(..)
    , negation
    , addition
    , subtraction
    , multiplication
    , division
    ) where

import Data.Proxy (Proxy(..))
import ExSql.Syntax.Class

data Arithmetic g a where
    Negation :: g a -> Arithmetic g a
    Addition :: g a -> g a -> Arithmetic g a
    Subtraction :: g a -> g a -> Arithmetic g a
    Multiplication :: g a -> g a -> Arithmetic g a
    Division :: g a -> g a -> Arithmetic g a

type Uop g a = UnaryOpType "Arithmetic" Arithmetic g a

type Bop g a = BinaryOpType "Arithmetic" Arithmetic g a

proxy :: Proxy "Arithmetic"
proxy = Proxy

negation :: Uop g a
negation = unaryOp proxy Negation

addition :: Bop g a
addition = binaryOp proxy Addition

subtraction :: Bop g a
subtraction = binaryOp proxy Subtraction

multiplication :: Bop g a
multiplication = binaryOp proxy Multiplication

division :: Bop g a
division = binaryOp proxy Division
