{-# LANGUAGE GADTs, RankNTypes, FlexibleContexts #-}
module ExSql.Syntax.Arithmetic
    ( Arithmetic(..)
    , negation
    , addition
    , subtraction
    , multiplication
    , division
    ) where

import ExSql.Syntax.Class

data Arithmetic g a where
    Negation :: g a -> Arithmetic g a
    Addition :: g a -> g a -> Arithmetic g a
    Subtraction :: g a -> g a -> Arithmetic g a
    Multiplication :: g a -> g a -> Arithmetic g a
    Division :: g a -> g a -> Arithmetic g a

instance Hoist Arithmetic where
    hoist f (Negation a) = Negation (f a)
    hoist f (Addition a0 a1) = Addition (f a0) (f a1)
    hoist f (Subtraction a0 a1) = Subtraction (f a0) (f a1)
    hoist f (Multiplication a0 a1) = Multiplication (f a0) (f a1)
    hoist f (Division a0 a1) = Division (f a0) (f a1)

type Uop g a = UnaryOpType Arithmetic g a a

type Bop g a = BinaryOpType Arithmetic g a a

negation :: Uop g a
negation = unaryOp Negation

addition :: Bop g a
addition = binaryOp Addition

subtraction :: Bop g a
subtraction = binaryOp Subtraction

multiplication :: Bop g a
multiplication = binaryOp Multiplication

division :: Bop g a
division = binaryOp Division
