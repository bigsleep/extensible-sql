{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE PolyKinds        #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}
module ExSql.Syntax.Class
    ( Ast(..)
    , Hoist(..)
    , HTraversable(..)
    , Node(..)
    , Expr(..)
    , UnaryOpType
    , BinaryOpType
    , unaryOp
    , binaryOp
    , hoistExpr
    ) where

import Data.Extensible ((:|)(..), Member, embed)
import Data.Extensible.HList (HList(..))
import qualified Data.Extensible.HList as HList (htraverse)
import Data.Functor.Identity (Identity(..))

class Ast g where
    type NodeTypes g :: [(* -> *) -> * -> *]
    mkAst :: (Monad m, Hoist v, Member (NodeTypes g) v) => m (v (g m) a) -> g m a
    hoistAst :: (Functor n) => (forall x. m x -> n x) -> g m a -> g n a

class Hoist (t :: (* -> *) -> k -> *) where
    hoist :: (forall x. f x -> g x) -> t f a -> t g a

class HTraversable (t :: (* -> *) -> k -> *) where
    htraverse :: Applicative g => (forall x. f x -> g (h x)) -> t f a -> g (t h a)

instance Hoist HList where
    hoist f = runIdentity . HList.htraverse (return . f)

instance HTraversable HList where
    htraverse = HList.htraverse

data Node m g a (f :: (* -> *) -> * -> *) where
    Node :: (Hoist f) => m (f g a) -> Node m g a f

newtype Expr xs m a = Expr { unExpr :: Node m (Expr xs m) a :| xs }

instance Ast (Expr xs) where
    type NodeTypes (Expr xs) = xs
    mkAst = Expr . embed . Node
    hoistAst = hoistExpr

hoistExpr :: (Functor n) => (forall x. m x -> n x) -> Expr xs m a -> Expr xs n a
hoistExpr f (Expr (EmbedAt p (Node m))) = Expr (EmbedAt p (Node (hoist (hoistExpr f) <$> f m)))

type UnaryOpType v g m a b = (Ast g, Hoist v, Monad m, Member (NodeTypes g) v) => g m a -> g m b

type BinaryOpType v g m a b = (Ast g, Hoist v, Monad m, Member (NodeTypes g) v) => g m a -> g m a -> g m b

unaryOp :: (Ast g, Hoist v, Monad m, Member (NodeTypes g) v) => (g m a -> v (g m) b) -> g m a -> g m b
unaryOp op = mkAst . return . op

binaryOp :: (Ast g, Hoist v, Monad m, Member (NodeTypes g) v) => (g m a -> g m a -> v (g m) b) -> g m a -> g m a -> g m b
binaryOp op a0 a1 = mkAst . return $ a0 `op` a1
