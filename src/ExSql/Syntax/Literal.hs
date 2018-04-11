{-# LANGUAGE KindSignatures, GADTs, RankNTypes, FlexibleContexts #-}
module ExSql.Syntax.Literal
    ( Literal(..)
    , int
    , bool
    ) where

import Data.Extensible (Member)
import ExSql.Syntax.Class

data Literal (g :: * -> *) a where
    LitInt :: Int -> Literal g Int
    LitBool :: Bool -> Literal g Bool

instance Hoist Literal where
    hoist _ (LitInt a) = LitInt a
    hoist _ (LitBool a) = LitBool a

type Constructor g m a = (Ast g, Monad m, Member (NodeTypes g) Literal) => a -> g m a

int :: Constructor g m Int
int = mkAst . return . LitInt

bool :: Constructor g m Bool
bool = mkAst . return . LitBool
