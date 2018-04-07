{-# LANGUAGE DataKinds, KindSignatures, GADTs, RankNTypes, FlexibleContexts #-}
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

type Constructor g a = (Ast g, Member (NodeTypes g) Literal) => a -> g a

int :: Constructor g Int
int = mkAst . LitInt

bool :: Constructor g Bool
bool = mkAst . LitBool
