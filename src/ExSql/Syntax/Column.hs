{-# LANGUAGE
    FlexibleContexts,
    GADTs,
    KindSignatures
#-}
module ExSql.Syntax.Column
    ( Column(..)
    , column
    , (.^)
    ) where

import Data.Extensible (Member)
import Database.Persist.Class (PersistEntity(..))
import ExSql.Syntax.Class
import ExSql.Syntax.Internal.Types (FieldRef)

data Column (g :: * -> *) a where
    Column :: (PersistEntity record) => FieldRef record -> EntityField record a -> Column g a

instance Hoist Column where
    hoist _ (Column t a) = Column t a

column :: (Ast g, Monad m, Member (NodeTypes g) Column, PersistEntity record)
    => FieldRef record -> EntityField record a -> g m a
column t = mkAst . return . Column t

(.^) :: (Ast g, Monad m, Member (NodeTypes g) Column, PersistEntity record)
    => FieldRef record -> EntityField record a -> g m a
(.^) = column

infixl 9 .^
