{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
module ExSql.Syntax.SelectQuery
    ( SelectQuery(..)
    , FieldsSelector(..)
    , SelectClause(..)
    , SelectClauses(..)
    , OrderType(..)
    , selectFrom
    , selectFromSub
    , resultAs
    , where_
    , orderBy
    , limit
    , offset
    ) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT)
import qualified Control.Monad.Trans.State.Strict as State (evalStateT, get,
                                                            mapStateT, put)
import Control.Monad.Trans.Writer.Strict (Writer)
import qualified Control.Monad.Trans.Writer.Strict as Writer (mapWriter,
                                                              runWriter, tell)
import Data.DList (DList)
import Data.Int (Int64)
import Data.Proxy (Proxy(..))
import Data.Semigroup (Semigroup(..))
import Database.Persist (Entity(..), PersistEntity(..), PersistField(..))
import ExSql.Syntax.Class
import ExSql.Syntax.Internal.Types (FieldAlias(..), PersistConvert, Ref(..),
                                    RelationAlias(..), Sel(..),
                                    SelWithAlias(..))

type family ResultType a where
    ResultType (a -> b) = ResultType b
    ResultType a = a

class KnownConstructor a where
    type ConstructorType a

data SResult a

instance KnownConstructor (SResult (a, b)) where
    type ConstructorType (SResult (a, b)) = (a -> b -> (a, b))

instance KnownConstructor (SResult (a, b, c)) where
    type ConstructorType (SResult (a, b, c)) = (a -> b -> c -> (a, b, c))

instance KnownConstructor (SResult (a, b, c, d)) where
    type ConstructorType (SResult (a, b, c, d)) = (a -> b -> c -> d -> (a, b, c, d))

instance KnownConstructor (SResult (Entity a)) where
    type ConstructorType (SResult (Entity a)) = (Entity a -> Entity a)

newtype SelectQuery (g :: * -> *) a = SelectQuery
    { unSelectQuery :: StateT (Int, Int) (Writer (SelectClauses g)) (FieldsSelector Ref a)
    }

data SelectClause (g :: * -> *) where
    Fields :: FieldsSelector (SelWithAlias g) a -> SelectClause g
    From :: (PersistEntity record) => RelationAlias (Entity record) -> SelectClause g
    FromSub :: Int -> SelectQuery g a -> SelectClause g
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
        h = Writer.mapWriter $ \(x, SelectClauses w) -> (x, SelectClauses (fmap (hoist' f) w))

hoist' :: (forall x. m x -> n x) -> SelectClause m -> SelectClause n
hoist' f (Fields a)    = Fields (hoist (hoist f) a)
hoist' _ (From a)      = From a
hoist' f (FromSub i a) = FromSub i (hoist f a)
hoist' f (Where a)     = Where (f a)
hoist' f (OrderBy a t) = OrderBy (f a) t
hoist' _ (Limit a)     = Limit a
hoist' _ (Offset a)    = Offset a
hoist' _ Initial       = Initial

data OrderType = Asc | Desc deriving (Show, Eq)

selectFrom :: forall a g record. (PersistEntity record) => (Ref (Entity record) -> SelectQuery g (Entity record) -> SelectQuery g a) -> SelectQuery g a
selectFrom f = SelectQuery $ do
    (i, j) <- State.get
    State.put (i + 1, j)
    let ref = RelationRef i :: Ref (Entity record)
        alias = RelationAlias i :: RelationAlias (Entity record)
        sref = id :$: ref
    lift . Writer.tell . SelectClauses . return . From $ alias
    unSelectQuery . f ref . SelectQuery . return $ sref

selectFromSub :: SelectQuery g b -> (FieldsSelector Ref b -> SelectQuery g b -> SelectQuery g a) -> SelectQuery g a
selectFromSub sub f = SelectQuery $ do
    (i, j) <- State.get
    State.put (i + 1, j)
    let (sref, _) = Writer.runWriter . flip State.evalStateT (0, 0) . unSelectQuery $ sub
        qref = qualifySelectorRef i sref
    lift . Writer.tell . SelectClauses . return . FromSub i $ sub
    unSelectQuery . f qref . SelectQuery . return $ qref

resultAs :: FieldsSelector (Sel g) a1 -> (FieldsSelector Ref a1 -> SelectQuery g a1 -> SelectQuery g a2) -> SelectQuery g a0 -> SelectQuery g a2
resultAs selector cont (SelectQuery pre) = SelectQuery $ do
    _ <- pre
    selectorWithAlias <- mkRef selector
    lift . Writer.tell . SelectClauses . return $ Fields selectorWithAlias
    let ref = hoist aliasToRef selectorWithAlias
    unSelectQuery . cont ref . SelectQuery . return $ ref

    where
    mkRef s = do
        (i, j) <- State.get
        let (selectorWithAlias, next) = mkSelectorWithAlias j s
        State.put (i, next)
        return selectorWithAlias

    aliasToRef :: SelWithAlias g a -> Ref a
    aliasToRef (Star' (RelationAlias i)) = RelationRef i
    aliasToRef (Sel' _ (FieldAlias i))   = FieldRef i

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

data FieldsSelector g x where
    (:$:) :: (KnownConstructor (SResult (ResultType b)), ConstructorType (SResult (ResultType b)) ~ (a -> b)) => (a -> b) -> g a -> FieldsSelector g b
    (:*:) :: (KnownConstructor (SResult (ResultType b))) => FieldsSelector g (a -> b) -> g a -> FieldsSelector g b

infixl 4 :$:, :*:

instance Hoist FieldsSelector where
    hoist f (g :$: a) = g :$: f a
    hoist f (s :*: a) = hoist f s :*: f a

mkSelectorWithAlias :: Int -> FieldsSelector (Sel g) a -> (FieldsSelector (SelWithAlias g) a, Int)
mkSelectorWithAlias i (f :$: Star a) = (f :$: Star' a, i)
mkSelectorWithAlias i (f :$: Sel a) = (f :$: Sel' a (FieldAlias i), i + 1)
mkSelectorWithAlias i (s :*: Star a) =
    let (r, next) = mkSelectorWithAlias i s
    in (r :*: Star' a, next)
mkSelectorWithAlias i (s :*: Sel a) =
    let (r, next) = mkSelectorWithAlias i s
    in (r :*: Sel' a (FieldAlias next), next + 1)

qualifySelectorRef :: Int -> FieldsSelector Ref a -> FieldsSelector Ref a
qualifySelectorRef tid (f :$: a) = f :$: qualifyRef tid a
qualifySelectorRef tid (s :*: a) = qualifySelectorRef tid s :*: qualifyRef tid a

qualifyRef :: Int -> Ref a -> Ref a
qualifyRef tid (RelationRef _)           = RelationRef tid
qualifyRef tid (FieldRef fid)            = QualifiedFieldRef tid fid
qualifyRef tid (QualifiedFieldRef _ fid) = QualifiedFieldRef tid fid
