{-# LANGUAGE
    FlexibleContexts,
    GADTs,
    KindSignatures
#-}
module ExSql.Syntax.SubSelect
    ( SubSelect(..)
    , subSelect
    , subSelectValues
    ) where

import Data.Extensible (Member)
import Database.Persist.Sql (Single)
import ExSql.Syntax.Class
import ExSql.Syntax.Internal.Types
import ExSql.Syntax.SelectQuery

data SubSelect (g :: * -> *) a where
    SubSelect :: SelectQuery g (Single a) -> SubSelect g a
    SubSelectValues :: SelectQuery g a -> SubSelect g (ValueList a)

instance Hoist SubSelect where
    hoist f (SubSelect query) = SubSelect (hoist f query)
    hoist f (SubSelectValues query) = SubSelectValues (hoist f query)

subSelect :: (Ast g, Monad m, Member (NodeTypes g) SubSelect)
    => SelectQuery (g m) (Single a) -> g m a
subSelect = mkAst . return . SubSelect

subSelectValues :: (Ast g, Monad m, Member (NodeTypes g) SubSelect)
    => SelectQuery (g m) a -> g m (ValueList a)
subSelectValues = mkAst . return . SubSelectValues
