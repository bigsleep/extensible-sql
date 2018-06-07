{-# LANGUAGE
    FlexibleContexts,
    GADTs,
    KindSignatures
#-}
module ExSql.Syntax.Field
    ( Field(..)
    , field
    ) where

import Data.Extensible (Member)
import Database.Persist.Class (PersistField(..))
import ExSql.Syntax.Class
import ExSql.Syntax.Internal.Types (FieldRef)

data Field (g :: * -> *) a where
    Field :: (PersistField a) => FieldRef a -> Field g a

instance Hoist Field where
    hoist _ (Field a) = Field a

field :: (Ast g, Monad m, Member (NodeTypes g) Field, PersistField a)
    => FieldRef a -> g m a
field = mkAst . return . Field
