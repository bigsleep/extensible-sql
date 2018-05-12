{-# LANGUAGE GADTs, RankNTypes, FlexibleContexts #-}
module ExSql.Syntax.In
    (
    ) where

import Data.Extensible (Member)
import ExSql.Syntax.Class
import ExSql.Syntax.Internal.Types (ValueList)

data In g a where
    In :: g a -> g (ValueList a) -> In g Bool
    In1 :: g a -> g (ValueList a) -> In g Bool
    In2 :: (g a0, g a1) -> g (ValueList (a0, a1)) -> In g Bool
    In3 :: (g a0, g a1, g a2) -> g (ValueList (a0, a1, a2)) -> In g Bool
    In4 :: (g a0, g a1, g a2, g a3) -> g (ValueList (a0, a1, a2, a3)) -> In g Bool
    In5 :: (g a0, g a1, g a2, g a3, g a4) -> g (ValueList (a0, a1, a2, a3, a4)) -> In g Bool
    In6 :: (g a0, g a1, g a2, g a3, g a4, g a5) -> g (ValueList (a0, a1, a2, a3, a4, a5)) -> In g Bool
    NotIn :: g a -> g (ValueList a) -> In g Bool
    NotIn1 :: g a -> g (ValueList a) -> In g Bool
    NotIn2 :: (g a0, g a1) -> g (ValueList (a0, a1)) -> In g Bool
    NotIn3 :: (g a0, g a1, g a2) -> g (ValueList (a0, a1, a2)) -> In g Bool
    NotIn4 :: (g a0, g a1, g a2, g a3) -> g (ValueList (a0, a1, a2, a3)) -> In g Bool
    NotIn5 :: (g a0, g a1, g a2, g a3, g a4) -> g (ValueList (a0, a1, a2, a3, a4)) -> In g Bool
    NotIn6 :: (g a0, g a1, g a2, g a3, g a4, g a5) -> g (ValueList (a0, a1, a2, a3, a4, a5)) -> In g Bool

instance Hoist In where
    hoist f (In a b) = In (f a) (f b)
    hoist f (In1 a b) = In1 (f a) (f b)
    hoist f (In2 (a0, a1) b) = In2 (f a0, f a1) (f b)
    hoist f (In3 (a0, a1, a2) b) = In3 (f a0, f a1, f a2) (f b)
    hoist f (In4 (a0, a1, a2, a3) b) = In4 (f a0, f a1, f a2, f a3) (f b)
    hoist f (In5 (a0, a1, a2, a3, a4) b) = In5 (f a0, f a1, f a2, f a3, f a4) (f b)
    hoist f (In6 (a0, a1, a2, a3, a4, a5) b) = In6 (f a0, f a1, f a2, f a3, f a4, f a5) (f b)
    hoist f (NotIn a b) = NotIn (f a) (f b)
    hoist f (NotIn1 a b) = NotIn1 (f a) (f b)
    hoist f (NotIn2 (a0, a1) b) = NotIn2 (f a0, f a1) (f b)
    hoist f (NotIn3 (a0, a1, a2) b) = NotIn3 (f a0, f a1, f a2) (f b)
    hoist f (NotIn4 (a0, a1, a2, a3) b) = NotIn4 (f a0, f a1, f a2, f a3) (f b)
    hoist f (NotIn5 (a0, a1, a2, a3, a4) b) = NotIn5 (f a0, f a1, f a2, f a3, f a4) (f b)
    hoist f (NotIn6 (a0, a1, a2, a3, a4, a5) b) = NotIn6 (f a0, f a1, f a2, f a3, f a4, f a5) (f b)

in_ :: (Ast g, Monad m, Member (NodeTypes g) In) => g m a -> g m (ValueList a) -> g m Bool
in_ = (.) (mkAst . return) . In

in1 :: (Ast g, Monad m, Member (NodeTypes g) In) =>  g m a -> g m (ValueList a) -> g m Bool
in1 = (.) (mkAst . return) . In1

in2 :: (Ast g, Monad m, Member (NodeTypes g) In) =>  (g m a0, g m a1) -> g m (ValueList (a0, a1)) -> g m Bool
in2 = (.) (mkAst . return) . In2

in3 :: (Ast g, Monad m, Member (NodeTypes g) In) =>  (g m a0, g m a1, g m a2) -> g m (ValueList (a0, a1, a2)) -> g m Bool
in3 = (.) (mkAst . return) . In3

in4 :: (Ast g, Monad m, Member (NodeTypes g) In) =>  (g m a0, g m a1, g m a2, g m a3) -> g m (ValueList (a0, a1, a2, a3)) -> g m Bool
in4 = (.) (mkAst . return) . In4

in5 :: (Ast g, Monad m, Member (NodeTypes g) In) =>  (g m a0, g m a1, g m a2, g m a3, g m a4) -> g m (ValueList (a0, a1, a2, a3, a4)) -> g m Bool
in5 = (.) (mkAst . return) . In5

in6 :: (Ast g, Monad m, Member (NodeTypes g) In) =>  (g m a0, g m a1, g m a2, g m a3, g m a4, g m a5) -> g m (ValueList (a0, a1, a2, a3, a4, a5)) -> g m Bool
in6 = (.) (mkAst . return) . In6

notIn :: (Ast g, Monad m, Member (NodeTypes g) In) =>  g m a -> g m (ValueList a) -> g m Bool
notIn = (.) (mkAst . return) . NotIn

notIn1 :: (Ast g, Monad m, Member (NodeTypes g) In) =>  g m a -> g m (ValueList a) -> g m Bool
notIn1 = (.) (mkAst . return) . NotIn1

notIn2 :: (Ast g, Monad m, Member (NodeTypes g) In) =>  (g m a0, g m a1) -> g m (ValueList (a0, a1)) -> g m Bool
notIn2 = (.) (mkAst . return) . NotIn2

notIn3 :: (Ast g, Monad m, Member (NodeTypes g) In) =>  (g m a0, g m a1, g m a2) -> g m (ValueList (a0, a1, a2)) -> g m Bool
notIn3 = (.) (mkAst . return) . NotIn3

notIn4 :: (Ast g, Monad m, Member (NodeTypes g) In) =>  (g m a0, g m a1, g m a2, g m a3) -> g m (ValueList (a0, a1, a2, a3)) -> g m Bool
notIn4 = (.) (mkAst . return) . NotIn4

notIn5 :: (Ast g, Monad m, Member (NodeTypes g) In) =>  (g m a0, g m a1, g m a2, g m a3, g m a4) -> g m (ValueList (a0, a1, a2, a3, a4)) -> g m Bool
notIn5 = (.) (mkAst . return) . NotIn5

notIn6 :: (Ast g, Monad m, Member (NodeTypes g) In) =>  (g m a0, g m a1, g m a2, g m a3, g m a4, g m a5) -> g m (ValueList (a0, a1, a2, a3, a4, a5)) -> g m Bool
notIn6 = (.) (mkAst . return) . NotIn6
