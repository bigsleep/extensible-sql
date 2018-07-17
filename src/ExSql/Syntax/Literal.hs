{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE RankNTypes       #-}
module ExSql.Syntax.Literal
    ( Literal(..)
    , int
    , bool
    , text
    , valueList
    ) where

import Data.Extensible (Member)
import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import ExSql.Syntax.Class
import ExSql.Syntax.Internal.Types (ValueList)

data Literal (g :: * -> *) a where
    LitInt :: Int64 -> Literal g Int64
    LitBool :: Bool -> Literal g Bool
    LitText :: Text -> Literal g Text
    LitValueList :: NonEmpty (g a) -> Literal g (ValueList a)

instance Hoist Literal where
    hoist _ (LitInt a)       = LitInt a
    hoist _ (LitBool a)      = LitBool a
    hoist _ (LitText a)      = LitText a
    hoist f (LitValueList a) = LitValueList (fmap f a)

type Constructor g m a = (Ast g, Monad m, Member (NodeTypes g) Literal) => a -> g m a

int :: Constructor g m Int64
int = mkAst . return . LitInt

bool :: Constructor g m Bool
bool = mkAst . return . LitBool

text ::  Constructor g m Text
text = mkAst . return . LitText

valueList :: (Ast g, Monad m, Member (NodeTypes g) Literal) => NonEmpty (g m a) -> g m (ValueList a)
valueList = mkAst . return . LitValueList
