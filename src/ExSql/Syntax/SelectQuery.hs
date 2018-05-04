{-# LANGUAGE GADTs, RankNTypes, FlexibleContexts, KindSignatures #-}
module ExSql.Syntax.SelectQuery
    ( SelectQuery(..)
    , Ref
    , selectFrom
    , where_
    ) where

import Database.Persist (Entity(..), PersistEntity(..))
import ExSql.Syntax.Class
import ExSql.Syntax.Internal.Types (Ref)

data SelectQuery (g :: * -> *) a where
    SelectFrom :: (PersistEntity record) => (Ref record -> SelectQuery g (Entity record) -> SelectQuery g a) -> SelectQuery g a
    Where :: g Bool -> SelectQuery g a -> SelectQuery g a
    Initial :: (PersistEntity a) => SelectQuery g (Entity a)

instance Hoist SelectQuery where
    hoist f (SelectFrom g) = SelectFrom $ \ref _ -> hoist f (g ref Initial)
    hoist f (Where cond query) = Where (f cond) (hoist f query)

selectFrom :: (PersistEntity record) => (Ref record -> SelectQuery g (Entity record) -> SelectQuery g a) -> SelectQuery g a
selectFrom = SelectFrom

where_ :: g Bool -> SelectQuery g a -> SelectQuery g a
where_ = Where
