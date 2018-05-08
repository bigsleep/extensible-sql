{-# LANGUAGE GADTs, RankNTypes, FlexibleContexts, KindSignatures, PatternSynonyms #-}
module ExSql.Syntax.SelectQuery
    ( SelectQuery(..)
    , Selector(..)
    , OrderType(..)
    , Ref
    , selectFrom
    , resultAs
    , where_
    ) where

import Database.Persist (Entity(..), PersistEntity(..), PersistField(..))
import ExSql.Syntax.Class
import ExSql.Syntax.Internal.Types (Ref, FieldRef, PersistConvert)

data SelectQuery (g :: * -> *) a where
    SelectFrom :: (PersistEntity record) => (Ref record -> SelectQuery g (Entity record) -> SelectQuery g a) -> SelectQuery g a
    ResultAs :: Selector g a -> (Selector FieldRef a -> SelectQuery g a -> SelectQuery g b) -> SelectQuery g c -> SelectQuery g b
    Where :: g Bool -> SelectQuery g a -> SelectQuery g a
    OrderBy :: g b -> OrderType -> SelectQuery g a -> SelectQuery g a
    Initial :: (PersistEntity a) => SelectQuery g (Entity a)
    Transform :: PersistConvert a -> SelectQuery g a

instance Hoist SelectQuery where
    hoist f (SelectFrom g) = SelectFrom $ \ref _ -> hoist f (g ref Initial)
    hoist f (Where cond query) = Where (f cond) (hoist f query)
    hoist _ Initial = Initial
    hoist _ (Transform f) = Transform f

data OrderType = Asc | Desc deriving (Show, Eq)

selectFrom :: (PersistEntity record) => (Ref record -> SelectQuery g (Entity record) -> SelectQuery g a) -> SelectQuery g a
selectFrom = SelectFrom

resultAs :: Selector g b -> (Selector FieldRef b -> SelectQuery g b -> SelectQuery g a) -> SelectQuery g c -> SelectQuery g a
resultAs = ResultAs

where_ :: g Bool -> SelectQuery g a -> SelectQuery g a
where_ = Where

orderBy :: g b -> OrderType -> SelectQuery g a -> SelectQuery g a
orderBy = OrderBy

data Selector g x where
    (:$) :: (PersistField a) => (a -> b) -> g a -> Selector g b
    (:*) :: (PersistField a) => Selector g (a -> b) -> g a -> Selector g b

infixl 4 :$, :*

instance Hoist Selector where
    hoist f (g :$ a) = g :$ (f a)
    hoist f (s :* a) = (hoist f s) :* (f a)
