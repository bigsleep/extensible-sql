{-# LANGUAGE DataKinds, ConstraintKinds, TypeFamilies, RankNTypes, FlexibleContexts, TypeOperators, GADTs #-}
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

import Data.Extensible (Member, (:|)(..), embed)
import GHC.TypeLits (Symbol)

class Ast g where
    type NodeTypes g :: [(* -> *) -> * -> *]
    mkAst :: (Hoist v, Member (NodeTypes g) v) => v g a -> g a

class Hoist (t :: (* -> *) -> * -> *) where
    hoist :: (forall x. f x -> g x) -> t f a -> t g a

data Node m g a (f :: (* -> *) -> * -> *) where
    Node :: (Hoist f) => m (f g a) -> Node m g a f

newtype Expr m xs a = Expr { unExpr :: Node m (Expr m xs) a :| xs }

instance (Monad m) => Ast (Expr m xs) where
    type NodeTypes (Expr m xs) = xs
    mkAst = Expr . embed . Node . return

hoistExpr :: (Monad n) => (forall x. m x -> n x) -> Expr m xs a -> Expr n xs a
hoistExpr f (Expr (EmbedAt p (Node m))) = Expr (EmbedAt p (Node (hoist (hoistExpr f) <$> f m)))

type UnaryOpType v g a b = (Ast g, Hoist v, Member (NodeTypes g) v) => g a -> g b

type BinaryOpType v g a b = (Ast g, Hoist v, Member (NodeTypes g) v) => g a -> g a -> g b

unaryOp :: (Ast g, Hoist v, Member (NodeTypes g) v) => (g a -> v g b) -> g a -> g b
unaryOp op = mkAst . op

binaryOp :: (Ast g, Hoist v, Member (NodeTypes g) v) => (g a -> g a -> v g b) -> g a -> g a -> g b
binaryOp op a0 a1 = mkAst $ a0 `op` a1
