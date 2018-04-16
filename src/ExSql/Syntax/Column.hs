{-# LANGUAGE KindSignatures, GADTs, FlexibleContexts #-}
module ExSql.Syntax.Column
    ( Column(..)
    , column
    ) where

import Control.Monad.Reader.Class (MonadReader(..))
import Data.Extensible (Member)
import Database.Persist.Class (PersistEntity(..))
import ExSql.Syntax.Class

data Column (g :: * -> *) a where
    Column :: (PersistEntity record) => EntityField record a -> Column g a

instance Hoist Column where
    hoist _ (Column a) = Column a

column
    :: (Ast g, MonadReader (proxy record) m, Member (NodeTypes g) Column, PersistEntity record)
    => EntityField record a -> g m a
column = mkAst . return . Column
