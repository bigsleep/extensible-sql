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
    mkAst :: (Monad m, Hoist v, Member (NodeTypes g) v) => m (v (g m) a) -> g m a

class Hoist (t :: (* -> *) -> * -> *) where
    hoist :: (forall x. f x -> g x) -> t f a -> t g a

data Node m g a (f :: (* -> *) -> * -> *) where
    Node :: (Hoist f) => m (f g a) -> Node m g a f

newtype Expr xs m a = Expr { unExpr :: Node m (Expr xs m) a :| xs }

instance Ast (Expr xs) where
    type NodeTypes (Expr xs) = xs
    mkAst = Expr . embed . Node

hoistExpr :: (Monad n) => (forall x. m x -> n x) -> Expr xs m a -> Expr xs n a
hoistExpr f (Expr (EmbedAt p (Node m))) = Expr (EmbedAt p (Node (hoist (hoistExpr f) <$> f m)))

type UnaryOpType v g m a b = (Ast g, Hoist v, Monad m, Member (NodeTypes g) v) => g m a -> g m b

type BinaryOpType v g m a b = (Ast g, Hoist v, Monad m, Member (NodeTypes g) v) => g m a -> g m a -> g m b

unaryOp :: (Ast g, Hoist v, Monad m, Member (NodeTypes g) v) => (g m a -> v (g m) b) -> g m a -> g m b
unaryOp op = mkAst . return . op

binaryOp :: (Ast g, Hoist v, Monad m, Member (NodeTypes g) v) => (g m a -> g m a -> v (g m) b) -> g m a -> g m a -> g m b
binaryOp op a0 a1 = mkAst . return $ a0 `op` a1
