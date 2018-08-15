{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}
module ExSql.Syntax.SelectQuery
    ( Field(..)
    , From(..)
    , SelectQuery(..)
    , SelectQueryInternal(..)
    , FieldsSelector(..)
    , SelectClause(..)
    , SelectClauses(..)
    , OrderType(..)
    , (.^)
    , column
    , field
    , groupBy
    , limit
    , offset
    , orderBy
    , aggResultAs
    , resultAs
    , select_
    , select
    , selectAgg_
    , selectAgg
    , selectInternal
    , from
    , fromSub
    , where_
    , afield
    , avg
    , max
    , min
    , stddev
    , variance
    , count
    , AggregateFunction(..)
    , AFields
    , ARef(..)
    , ARefs
    ) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.State.Strict (StateT)
import qualified Control.Monad.Trans.State.Strict as State (evalStateT, get,
                                                            mapStateT, put)
import Control.Monad.Trans.Writer.Strict (Writer)
import qualified Control.Monad.Trans.Writer.Strict as Writer (mapWriter,
                                                              runWriter, tell)
import Data.DList (DList)
import Data.Extensible (Member)
import qualified Data.Extensible.HList as HList (HList(..), htraverse)
import Data.Functor.Identity (Identity(..))
import Data.Int (Int64)
import Data.Semigroup (Semigroup(..))
import Data.Text (Text)
import Database.Persist (Entity(..), PersistEntity(..), PersistField(..),
                         PersistValue)
import ExSql.Syntax.Class
import ExSql.Syntax.Internal.SelectQueryStage
import ExSql.Syntax.Internal.Types
import Prelude hiding (max, min)

data SelectQuery g a where
    SelectQuery :: SelectQueryInternal s g a -> SelectQuery g a

type SelectQueryM g = StateT (Int, Int) (Writer (SelectClauses g))

newtype SelectQueryInternal stage (g :: * -> *) a = SelectQueryInternal
    { unSelectQueryInternal :: SelectQueryM g (FieldsSelector Ref a)
    }

data From g a where
    FromEntity :: PersistEntity record => Int -> proxy (Entity record) -> From g (Entity record)
    FromSubQuery :: Int -> SelectQuery g a -> From g a

data SelectClause (g :: * -> *) where
    Fields :: FieldsSelector (SelWithAlias g) a -> SelectClause g
    From :: From g a -> SelectClause g
    Join :: From g a -> Maybe (g Bool) -> SelectClause g
    Where :: g Bool -> SelectClause g
    GroupBy :: AFields g xs -> SelectClause g
    OrderBy :: g b -> OrderType -> SelectClause g
    Limit :: Int64 -> SelectClause g
    Offset :: Int64 -> SelectClause g
    Initial :: SelectClause g

newtype SelectClauses g = SelectClauses (DList (SelectClause g))
    deriving (Semigroup, Monoid)

instance Hoist SelectQuery where
    hoist f (SelectQuery a) = SelectQuery (hoist f a)

instance Hoist (SelectQueryInternal s) where
    hoist f (SelectQueryInternal a) = SelectQueryInternal $ State.mapStateT h a
        where
        h = Writer.mapWriter $ \(x, SelectClauses w) -> (x, SelectClauses (fmap (hoist' f) w))

instance Hoist From where
    hoist _ (FromEntity i a)   = FromEntity i a
    hoist f (FromSubQuery i a) = FromSubQuery i (hoist f a)

data OrderType = Asc | Desc deriving (Show, Eq)

data Field (g :: * -> *) a where
    Field :: (PersistField a) => Ref a -> Field g a
    Column :: (PersistEntity record) => RRef (Entity record) -> EntityField record a -> Field g a

instance Hoist Field where
    hoist _ (Field a)    = Field a
    hoist _ (Column t a) = Column t a

newtype ARef g a = ARef (Field g a)

instance Hoist ARef where
    hoist f (ARef a) = ARef (hoist f a)

data AggregateFunction g a where
    AggFunction :: Text -> g a -> AggregateFunction g a
    AggField :: ARef g a -> AggregateFunction g a
    Count :: g a -> AggregateFunction g Int64

instance Hoist AggregateFunction where
    hoist f (AggFunction name e) = AggFunction name (f e)
    hoist f (AggField ref)       = AggField (hoist f ref)
    hoist f (Count e)            = Count (f e)

type AFields g xs = HList.HList (Field g) xs

type ARefs g xs = HList.HList (ARef g) xs

select_ :: (SelectQueryInternal Neutral g [PersistValue] -> SelectQueryInternal Neutral g a) -> SelectQuery g a
select_ = SelectQuery . selectInternal

select :: (SelectQueryInternal Neutral g [PersistValue] -> SelectQueryInternal FieldsSpecified g a) -> SelectQuery g a
select = SelectQuery . selectInternal

selectAgg_ :: (SelectQueryInternal Neutral g [PersistValue] -> SelectQueryInternal Aggregated g a) -> SelectQuery g a
selectAgg_ = SelectQuery . selectInternal

selectAgg :: (SelectQueryInternal Neutral g [PersistValue] -> SelectQueryInternal AggFieldsSpecified g a) -> SelectQuery g a
selectAgg = SelectQuery . selectInternal

selectInternal :: (SelectQueryInternal s0 g [PersistValue] -> SelectQueryInternal s1 g a) -> SelectQueryInternal s1 g a
selectInternal f = f . SelectQueryInternal . return $ Raw

from
    :: (PersistEntity record)
    => (RRef (Entity record) -> RelationAlias (Entity record) -> SelectQueryInternal Neutral g (Entity record) -> SelectQueryInternal s1 g a)
    -> SelectQueryInternal Neutral g x
    -> SelectQueryInternal s1 g a
from f (SelectQueryInternal pre) = SelectQueryInternal $ do
    i <- prepareFrom pre
    let (rref, ref, alias, sref) = mkEntityRefs i
    tellSelectClause . From $ FromEntity i alias
    unSelectQueryInternal . f rref alias . SelectQueryInternal . return $ sref

fromSub
    :: SelectQuery g b
    -> (FieldsSelector Ref b -> RelationAlias b -> SelectQueryInternal Neutral g b -> SelectQueryInternal s1 g a)
    -> SelectQueryInternal Neutral g x
    -> SelectQueryInternal s1 g a
fromSub sub @ subq f (SelectQueryInternal pre) = SelectQueryInternal $ do
    i <- prepareFrom pre
    let qref = qualifySelectorRef i . evalSubQueryRef $ subq
        alias = RelationAliasSub i qref
    tellSelectClause . From $ FromSubQuery i sub
    unSelectQueryInternal . f qref alias . SelectQueryInternal . return $ qref

joinOn
    :: (PersistEntity record)
    => (RRef (Entity record) -> g Bool)
    -> (RRef (Entity record) -> RelationAlias (Entity record) -> SelectQueryInternal Neutral g (Entity record) -> SelectQueryInternal s1 g a)
    -> SelectQueryInternal Neutral g x
    -> SelectQueryInternal s1 g a
joinOn cond f (SelectQueryInternal pre) = SelectQueryInternal $ do
    i <- prepareFrom pre
    let (rref, ref, alias, sref) = mkEntityRefs i
    tellSelectClause $ Join (FromEntity i alias) (Just $ cond rref)
    unSelectQueryInternal . f rref alias . SelectQueryInternal . return $ sref

joinOnSub
    :: SelectQuery g b
    -> (FieldsSelector Ref b -> g Bool)
    -> (FieldsSelector Ref b -> RelationAlias b -> SelectQueryInternal Neutral g b -> SelectQueryInternal s1 g a)
    -> SelectQueryInternal Neutral g x
    -> SelectQueryInternal s1 g a
joinOnSub sub @ subq cond f (SelectQueryInternal pre) = SelectQueryInternal $ do
    i <- prepareFrom pre
    let qref = qualifySelectorRef i . evalSubQueryRef $ subq
        alias = RelationAliasSub i qref
    tellSelectClause $ Join (FromSubQuery i sub) (Just $ cond qref)
    unSelectQueryInternal . f qref alias . SelectQueryInternal . return $ qref

resultAs
    :: (Ast g, expr ~ g Identity)
    => FieldsSelector (Sel expr) a1
    -> (FieldsSelector Ref a1 -> SelectQueryInternal FieldsSpecified expr a1 -> SelectQueryInternal FieldsSpecified expr a2)
    -> SelectQueryInternal Neutral expr a0
    -> SelectQueryInternal FieldsSpecified expr a2
resultAs = resultAsInternal

aggResultAs
    :: (Ast g, expr0 ~ g (ReaderT Aggregated Identity), expr1 ~ g Identity)
    => FieldsSelector (Sel expr0) a1
    -> (FieldsSelector Ref a1 -> SelectQueryInternal AggFieldsSpecified expr1 a1 -> SelectQueryInternal AggFieldsSpecified expr1 a2)
    -> SelectQueryInternal Aggregated expr1 a0
    -> SelectQueryInternal AggFieldsSpecified expr1 a2
aggResultAs selector = resultAsInternal selector'
    where
    selector' = hoist (hoist (hoistAst (`runReaderT` Aggregated))) selector

resultAsInternal
    :: FieldsSelector (Sel g) a1
    -> (FieldsSelector Ref a1 -> SelectQueryInternal stage1 g a1 -> SelectQueryInternal stage1 g a2)
    -> SelectQueryInternal stage0 g a0
    -> SelectQueryInternal stage1 g a2
resultAsInternal selector cont (SelectQueryInternal pre) = SelectQueryInternal $ do
    _ <- pre
    selectorWithAlias <- mkRef selector
    lift . Writer.tell . SelectClauses . return $ Fields selectorWithAlias
    let ref = hoist aliasToRef selectorWithAlias
    unSelectQueryInternal . cont ref . SelectQueryInternal . return $ ref

    where
    mkRef s = do
        (i, j) <- State.get
        let (selectorWithAlias, next) = mkSelectorWithAlias j s
        State.put (i, next)
        return selectorWithAlias

    aliasToRef :: SelWithAlias g a -> Ref a
    aliasToRef (Star' (RelationAlias i))        = RelationRef (RRef i)
    aliasToRef (Star' (RelationAliasSub i ref)) = RelationRef (RRefSub i ref)
    aliasToRef (Sel' _ (FieldAlias i))          = FieldRef (FRef i)

where_ :: g Bool -> SelectQueryInternal s g a -> SelectQueryInternal s g a
where_ a (SelectQueryInternal q) = SelectQueryInternal $ do
    r <- q
    lift . Writer.tell . SelectClauses . return $ Where a
    return r

groupBy
    :: (Ast g, Functor m, Member (NodeTypes g) AggregateFunction)
    => AFields (g m) xs
    -> (ARefs (g m) xs -> SelectQueryInternal Aggregated (g m) [PersistValue] -> SelectQueryInternal s (g m) a1)
    -> SelectQueryInternal Neutral (g m) a0
    -> SelectQueryInternal s (g m) a1
groupBy fields cont pre = SelectQueryInternal $ do
    _ <- unSelectQueryInternal pre
    let ref = afieldsToARefs fields
    lift . Writer.tell . SelectClauses . return $ GroupBy fields
    unSelectQueryInternal . cont ref . SelectQueryInternal . return $ Raw

orderBy :: g b -> OrderType -> SelectQueryInternal s g a -> SelectQueryInternal s g a
orderBy a t (SelectQueryInternal q) = SelectQueryInternal $ do
    r <- q
    lift . Writer.tell . SelectClauses . return $ OrderBy a t
    return r

limit :: Int64 -> SelectQueryInternal s g a -> SelectQueryInternal s g a
limit a (SelectQueryInternal q) = SelectQueryInternal $ do
    r <- q
    lift . Writer.tell . SelectClauses . return . Limit $ a
    return r

offset :: Int64 -> SelectQueryInternal s g a -> SelectQueryInternal s g a
offset a (SelectQueryInternal q) = SelectQueryInternal $ do
    r <- q
    lift . Writer.tell . SelectClauses . return . Offset $ a
    return r

field :: (Ast g, m ~ Identity, Member (NodeTypes g) Field, PersistField a)
    => Ref a -> g m a
field = mkAst . return . Field

column :: (Ast g, m ~ Identity, Member (NodeTypes g) Field, PersistEntity record)
    => RRef (Entity record) -> EntityField record a -> g m a
column t = mkAst . return . Column t

(.^) :: (Ast g, m ~ Identity, Member (NodeTypes g) Field, PersistEntity record)
    => RRef (Entity record) -> EntityField record a -> g m a
(.^) = column

infixl 9 .^

type AggFunctionType g n a b = (Ast g, Monad n, Member (NodeTypes g) AggregateFunction) => g (ReaderT Aggregated n) a -> g (ReaderT Aggregated n) b

afield :: (Ast g, Monad n, Member (NodeTypes g) AggregateFunction) => ARef (g n) a -> g (ReaderT Aggregated n) a
afield = mkAst . return . AggField . hoist (hoistAst lift)

avg :: AggFunctionType g n a a
avg = mkAst . return . AggFunction "avg"

max :: AggFunctionType g n a a
max = mkAst . return . AggFunction "max"

min :: AggFunctionType g n a a
min = mkAst . return . AggFunction "min"

sum :: AggFunctionType g n a a
sum = mkAst . return . AggFunction "sum"

stddev :: AggFunctionType g n a a
stddev = mkAst . return . AggFunction "stddev"

variance :: AggFunctionType g n a a
variance = mkAst . return . AggFunction "variance"

count :: AggFunctionType g n a Int64
count = mkAst . return . Count

hoist' :: (forall x. m x -> n x) -> SelectClause m -> SelectClause n
hoist' f (Fields a)    = Fields (hoist (hoist f) a)
hoist' f (From a)      = From (hoist f a)
hoist' f (Join a cond) = Join (hoist f a) (f <$> cond)
hoist' f (Where a)     = Where (f a)
hoist' f (GroupBy fs)  = GroupBy . runIdentity . HList.htraverse (Identity . hoist f) $ fs
hoist' f (OrderBy a t) = OrderBy (f a) t
hoist' _ (Limit a)     = Limit a
hoist' _ (Offset a)    = Offset a
hoist' _ Initial       = Initial

mkSelectorWithAlias :: Int -> FieldsSelector (Sel g) a -> (FieldsSelector (SelWithAlias g) a, Int)
mkSelectorWithAlias i Raw = (Raw, i)
mkSelectorWithAlias i (f :$: Star a) = (f :$: Star' a, i)
mkSelectorWithAlias i (f :$: Sel a) = (f :$: Sel' a (FieldAlias i), i + 1)
mkSelectorWithAlias i (s :*: Star a) =
    let (r, next) = mkSelectorWithAlias i s
    in (r :*: Star' a, next)
mkSelectorWithAlias i (s :*: Sel a) =
    let (r, next) = mkSelectorWithAlias i s
    in (r :*: Sel' a (FieldAlias next), next + 1)

qualifySelectorRef :: Int -> FieldsSelector Ref a -> FieldsSelector Ref a
qualifySelectorRef _ Raw = Raw
qualifySelectorRef tid (f :$: a) = f :$: qualifyRef tid a
qualifySelectorRef tid (s :*: a) = qualifySelectorRef tid s :*: qualifyRef tid a

qualifyRef :: Int -> Ref a -> Ref a
qualifyRef tid (RelationRef RRef {})         = RelationRef (RRef tid)
qualifyRef tid (RelationRef (RRefSub _ ref)) = RelationRef (RRefSub tid ref)
qualifyRef tid (FieldRef (FRef fid))         = FieldRef (QRef tid fid)
qualifyRef tid (FieldRef (QRef _ fid))       = FieldRef (QRef tid fid)

afieldsToARefs :: AFields g xs -> ARefs g xs
afieldsToARefs = runIdentity . HList.htraverse f
    where
    f a = Identity (ARef a)

prepareFrom :: SelectQueryM g b -> SelectQueryM g Int
prepareFrom pre = do
    _ <- pre
    (i, j) <- State.get
    State.put (i + 1, j)
    return i

evalSubQueryRef :: SelectQuery g a -> FieldsSelector Ref a
evalSubQueryRef (SelectQuery a) =
    fst . Writer.runWriter . flip State.evalStateT (0, 0) . unSelectQueryInternal $ a

tellSelectClause :: SelectClause g -> SelectQueryM g ()
tellSelectClause = lift . Writer.tell . SelectClauses . return

mkEntityRefs :: (PersistEntity a) => Int -> (RRef (Entity a), Ref (Entity a), RelationAlias (Entity a), FieldsSelector Ref (Entity a))
mkEntityRefs i =
    let rref = RRef i
        ref = RelationRef rref
        alias = RelationAlias i
        sref = id :$: ref
    in (rref, ref, alias, sref)
