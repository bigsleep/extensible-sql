{-# LANGUAGE KindSignatures, GADTs, RankNTypes, FlexibleContexts #-}
module ExSql.Syntax.Literal
    ( Literal(..)
    , int
    , bool
    ) where

import Data.Extensible (Member)
import Data.Int (Int64)
import ExSql.Syntax.Class

data Literal (g :: * -> *) a where
    LitInt :: Int64 -> Literal g Int64
    LitBool :: Bool -> Literal g Bool

instance Hoist Literal where
    hoist _ (LitInt a) = LitInt a
    hoist _ (LitBool a) = LitBool a

type Constructor g m a = (Ast g, Monad m, Member (NodeTypes g) Literal) => a -> g m a

int :: Constructor g m Int64
int = mkAst . return . LitInt

bool :: Constructor g m Bool
bool = mkAst . return . LitBool
