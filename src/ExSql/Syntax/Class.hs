{-# LANGUAGE DataKinds, ConstraintKinds, TypeFamilies, RankNTypes, FlexibleContexts, TypeOperators #-}
module ExSql.Syntax.Class
    ( Ast(..)
    , Node(..)
    , Expr(..)
    , UnaryOpType
    , BinaryOpType
    , unaryOp
    , binaryOp
    ) where

import Data.Extensible (Assoc, Associate, (:|))
import Data.Proxy (Proxy(..))
import GHC.TypeLits (Symbol)

class Ast g where
    type NodeTypes g :: [Assoc Symbol ((* -> *) -> * -> *)]
    mkAst :: (Associate k v (NodeTypes g)) => Proxy k -> v g a -> g a

newtype Node g a (f :: (* -> *) -> * -> *) = Node { unNode :: f g a }

newtype Expr xs a = Expr { unExpr :: Node (Expr xs) a :| xs }

type UnaryOpType k v g a = (Ast g, Associate k v (NodeTypes g)) => g a -> g a

type BinaryOpType k v g a = (Ast g, Associate k v (NodeTypes g)) => g a -> g a -> g a

unaryOp :: (Ast g, Associate k v (NodeTypes g)) => Proxy k -> (g a -> v g a) -> g a -> g a
unaryOp k op = mkAst k . op

binaryOp :: (Ast g, Associate k v (NodeTypes g)) => Proxy k -> (g a -> g a -> v g a) -> g a -> g a -> g a
binaryOp k op a0 a1 = mkAst k $ a0 `op` a1
