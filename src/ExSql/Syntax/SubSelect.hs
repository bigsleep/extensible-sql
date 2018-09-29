{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE KindSignatures   #-}
module ExSql.Syntax.SubSelect
    ( SubSelect(..)
    , subSelect
    , subSelectValues
    ) where

import Data.Extensible (Member)
import Database.Persist.Sql (Single)
import ExSql.Syntax.Class
import ExSql.Syntax.Internal

data SubSelect (g :: * -> *) a where
    SubSelect :: SelectQuery t g (Single a) -> SubSelect g (SelectResultType t a)
    SubSelectValues :: SelectQuery t g a -> SubSelect g (ValueList (SelectResultType t a))

instance Hoist SubSelect where
    hoist f (SubSelect query)       = SubSelect (hoist f query)
    hoist f (SubSelectValues query) = SubSelectValues (hoist f query)

subSelect :: (Ast g, Monad m, Member (NodeTypes g) SubSelect)
    => SelectQuery t (g m) (Single a) -> g m (SelectResultType t a)
subSelect = mkAst . return . SubSelect

subSelectValues :: (Ast g, Monad m, Member (NodeTypes g) SubSelect)
    => SelectQuery t (g m) a -> g m (ValueList (SelectResultType t a))
subSelectValues = mkAst . return . SubSelectValues
