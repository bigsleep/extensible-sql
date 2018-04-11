{-# LANGUAGE DataKinds, ConstraintKinds, TypeFamilies, RankNTypes, FlexibleContexts, TypeOperators #-}
module ExSql.Syntax.Class
    ( Ast(..)
    , Hoist(..)
    , Node(..)
    , Expr(..)
    , UnaryOpType
    , BinaryOpType
    , unaryOp
    , binaryOp
    ) where

import Data.Extensible (Member, (:|), embed)
import GHC.TypeLits (Symbol)

class Ast g where
    type NodeTypes g :: [(* -> *) -> * -> *]
    mkAst :: (Member (NodeTypes g) v) => v g a -> g a

class Hoist (t :: (* -> *) -> * -> *) where
    hoist :: (forall x. f x -> g x) -> t f a -> t g a

newtype Node g a (f :: (* -> *) -> * -> *) = Node { unNode :: f g a }

newtype Expr xs a = Expr { unExpr :: Node (Expr xs) a :| xs }

instance Ast (Expr xs) where
    type NodeTypes (Expr xs) = xs
    mkAst = Expr . embed . Node

type UnaryOpType v g a b = (Ast g, Member (NodeTypes g) v) => g a -> g b

type BinaryOpType v g a b = (Ast g, Member (NodeTypes g) v) => g a -> g a -> g b

unaryOp :: (Ast g, Member (NodeTypes g) v) => (g a -> v g b) -> g a -> g b
unaryOp op = mkAst . op

binaryOp :: (Ast g, Member (NodeTypes g) v) => (g a -> g a -> v g b) -> g a -> g a -> g b
binaryOp op a0 a1 = mkAst $ a0 `op` a1
