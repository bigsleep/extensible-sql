{-# LANGUAGE KindSignatures, GADTs, FlexibleContexts, RankNTypes #-}
module ExSql.Syntax.Select
    ( Select(..)
    , select
    ) where

import Control.Monad.Reader.Class (MonadReader(..))
import Data.Extensible (Member)
import Database.Persist.Class (PersistEntity(..))
import ExSql.Syntax.Class

data Select g a where
    Select :: (PersistEntity record, Monad proxy) => proxy record -> g Bool -> Select g record

instance Hoist Select where
    hoist f (Select target cond) = Select target (f cond)

select
    :: (Ast g, MonadReader (proxy record) m, Member (NodeTypes g) Select, PersistEntity record, Monad proxy)
    => proxy record -> g m Bool -> g m record
select target cond = mkAst . return $ Select target cond
