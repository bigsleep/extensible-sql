{-# LANGUAGE
    FlexibleContexts,
    GADTs,
    GeneralizedNewtypeDeriving,
    KindSignatures,
    PatternSynonyms,
    RankNTypes,
    ScopedTypeVariables
#-}
module ExSql.Syntax.SelectQuery
    ( SelectQuery(..)
    , Selector(..)
    , SelectClause(..)
    , SelectClauses(..)
    , OrderType(..)
    , Ref
    , selectFrom
    , resultAs
    , where_
    , orderBy
    , limit
    , offset
    ) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT)
import qualified Control.Monad.Trans.State.Strict as State (get, put, mapStateT)
import Control.Monad.Trans.Writer.Strict (Writer)
import qualified Control.Monad.Trans.Writer.Strict as Writer (tell, mapWriter)
import Data.Int (Int64)
import Data.DList (DList)
import Data.Functor.Product (Product(..))
import Data.Proxy (Proxy(..))
import Data.Semigroup (Semigroup(..))
import Database.Persist (Entity(..), PersistEntity(..), PersistField(..))
import ExSql.Syntax.Class
import ExSql.Syntax.Internal.Types (Ref(..), FieldAlias(..), FieldRef(..), PersistConvert)

newtype SelectQuery (g :: * -> *) a = SelectQuery
    { unSelectQuery :: StateT (Int, Int) (Writer (SelectClauses g)) (Selector FieldRef a)
    }

data SelectClause (g :: * -> *) where
    Fields :: Selector (Product g FieldAlias) a -> SelectClause g
    From :: (PersistEntity record) => Ref (Entity record) -> SelectClause g
    Where :: g Bool -> SelectClause g
    OrderBy :: g b -> OrderType -> SelectClause g
    Limit :: Int64 -> SelectClause g
    Offset :: Int64 -> SelectClause g
    Initial :: SelectClause g

newtype SelectClauses g = SelectClauses (DList (SelectClause g))
    deriving (Semigroup, Monoid)

instance Hoist SelectQuery where
    hoist f (SelectQuery a) = SelectQuery $ State.mapStateT h a
        where
        h = Writer.mapWriter $ \(x, (SelectClauses w)) -> (x, SelectClauses (fmap (hoist' f) w))

hoist' :: (forall x. m x -> n x) -> SelectClause m -> SelectClause n
hoist' f (Fields a) = Fields (hoist (\(Pair x y) -> Pair (f x) y) a)
hoist' _ (From a) = From a
hoist' f (Where a) = Where (f a)
hoist' f (OrderBy a t) = OrderBy (f a) t
hoist' _ (Limit a) = Limit a
hoist' _ (Offset a) = Offset a
hoist' _ Initial = Initial

data OrderType = Asc | Desc deriving (Show, Eq)

selectFrom :: forall a g record. (PersistEntity record) => (Selector FieldRef (Entity record) -> SelectQuery g (Entity record) -> SelectQuery g a) -> SelectQuery g a
selectFrom f = SelectQuery $ do
    (i, j) <- State.get
    State.put (i + 1, j)
    let ref = Ref i :: Ref (Entity record)
        sref = Sel ref
    lift . Writer.tell . SelectClauses . return . From $ ref
    unSelectQuery . f sref . SelectQuery . return $ sref

resultAs :: Selector g b -> (Selector FieldRef b -> SelectQuery g b -> SelectQuery g a) -> SelectQuery g c -> SelectQuery g a
resultAs selector cont (SelectQuery pre) = SelectQuery $ do
    _ <- pre
    selectorWithAlias <- mkRef selector
    lift . Writer.tell . SelectClauses . return $ Fields selectorWithAlias
    let ref = hoist (\(Pair _ a) -> aliasToRef a) selectorWithAlias
    unSelectQuery . cont ref . SelectQuery . return $ ref

    where
    mkRef s = do
        (i, j) <- State.get
        let (selectorWithAlias, next) = mkSelectorFieldAlias j s
        State.put (i, next)
        return selectorWithAlias

    aliasToRef :: FieldAlias a -> FieldRef a
    aliasToRef (FieldAlias a) = FieldRef a

where_ :: g Bool -> SelectQuery g a -> SelectQuery g a
where_ a (SelectQuery q) = SelectQuery $ do
    r <- q
    lift . Writer.tell . SelectClauses . return $ Where a
    return r

orderBy :: g b -> OrderType -> SelectQuery g a -> SelectQuery g a
orderBy a t (SelectQuery q) = SelectQuery $ do
    r <- q
    lift . Writer.tell . SelectClauses . return $ OrderBy a t
    return r

limit :: Int64 -> SelectQuery g a -> SelectQuery g a
limit a (SelectQuery q) = SelectQuery $ do
    r <- q
    lift . Writer.tell . SelectClauses . return . Limit $ a
    return r

offset :: Int64 -> SelectQuery g a -> SelectQuery g a
offset a (SelectQuery q) = SelectQuery $ do
    r <- q
    lift . Writer.tell . SelectClauses . return . Offset $ a
    return r

data Selector g x where
    Sel :: (PersistEntity a) => Ref (Entity a) -> Selector g (Entity a)
    (:$) :: (PersistField a) => (a -> b) -> g a -> Selector g b
    (:*) :: (PersistField a) => Selector g (a -> b) -> g a -> Selector g b

infixl 4 :$, :*

instance Hoist Selector where
    hoist _ (Sel a) = Sel a
    hoist f (g :$ a) = g :$ (f a)
    hoist f (s :* a) = (hoist f s) :* (f a)

mkSelectorFieldAlias :: Int -> Selector g a -> (Selector (Product g FieldAlias) a, Int)
mkSelectorFieldAlias i (Sel a) = (Sel a, i)
mkSelectorFieldAlias i (f :$ a) = (f :$ Pair a (toFieldAlias i a), i + 1)
mkSelectorFieldAlias i (s :* a) =
    let (r, next) = mkSelectorFieldAlias i s
    in (r :* Pair a (toFieldAlias next a), next + 1)

toFieldAlias :: (PersistField a) => Int -> g a -> FieldAlias a
toFieldAlias index _ = FieldAlias index
