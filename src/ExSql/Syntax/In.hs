{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE RankNTypes       #-}
module ExSql.Syntax.In
    ( In(..)
    , in_
    ) where

import Data.Extensible (Member)
import ExSql.Syntax.Class
import ExSql.Syntax.Internal.Row (Row(..))
import ExSql.Syntax.Internal.Types (ValueList)

data In g a where
    In :: Row g a -> g (ValueList a) -> In g Bool

instance Hoist In where
    hoist f (In a b) = In (hoist f a) (f b)

in_ :: (Ast g, Monad m, Member (NodeTypes g) In) => Row (g m) a -> g m (ValueList a) -> g m Bool
in_ = (.) (mkAst . return) . In
