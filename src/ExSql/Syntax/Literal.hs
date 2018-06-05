{-# LANGUAGE KindSignatures, GADTs, RankNTypes, FlexibleContexts #-}
module ExSql.Syntax.Literal
    ( Literal(..)
    , int
    , bool
    , valueList
    ) where

import Data.Extensible (Member)
import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty)
import ExSql.Syntax.Class
import ExSql.Syntax.Internal.Types (ValueList)

data Literal (g :: * -> *) a where
    LitInt :: Int64 -> Literal g Int64
    LitBool :: Bool -> Literal g Bool
    LitValueList :: NonEmpty (g a) -> Literal g (ValueList a)

instance Hoist Literal where
    hoist _ (LitInt a) = LitInt a
    hoist _ (LitBool a) = LitBool a
    hoist f (LitValueList a) = LitValueList (fmap f a)

type Constructor g m a = (Ast g, Monad m, Member (NodeTypes g) Literal) => a -> g m a

int :: Constructor g m Int64
int = mkAst . return . LitInt

bool :: Constructor g m Bool
bool = mkAst . return . LitBool

valueList :: (Ast g, Monad m, Member (NodeTypes g) Literal) => NonEmpty (g m a) -> g m (ValueList a)
valueList = mkAst . return . LitValueList
