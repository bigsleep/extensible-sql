{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
module ExSql.Syntax.SelectQuery
    ( Field(..)
    , SelectQuery(..)
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
    , resultAs
    , selectFrom
    , selectFromSub
    , where_
    , avg
    , max
    , min
    , stddev
    , variance
    , count
    , AFields
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

newtype SelectQuery stage (g :: * -> *) a = SelectQuery
    { unSelectQuery :: StateT (Int, Int) (Writer (SelectClauses g)) (FieldsSelector Ref a)
    }

data SelectClause (g :: * -> *) where
    Fields :: FieldsSelector (SelWithAlias g) a -> SelectClause g
    From :: (PersistEntity record) => Int -> proxy (Entity record) -> SelectClause g
    FromSub :: Int -> SelectQuery s g a -> SelectClause g
    Where :: g Bool -> SelectClause g
    GroupBy :: AFields g xs -> SelectClause g
    OrderBy :: g b -> OrderType -> SelectClause g
    Limit :: Int64 -> SelectClause g
    Offset :: Int64 -> SelectClause g
    Initial :: SelectClause g

newtype SelectClauses g = SelectClauses (DList (SelectClause g))
    deriving (Semigroup, Monoid)

instance Hoist (SelectQuery s) where
    hoist f (SelectQuery a) = SelectQuery $ State.mapStateT h a
        where
        h = Writer.mapWriter $ \(x, SelectClauses w) -> (x, SelectClauses (fmap (hoist' f) w))

data OrderType = Asc | Desc deriving (Show, Eq)

data Field (g :: * -> *) a where
    Field :: (PersistField a) => FRef a -> Field g a
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

selectFrom
    :: forall a g record stage. (PersistEntity record)
    => (Ref (Entity record)-> RelationAlias (Entity record) -> SelectQuery Neutral g (Entity record) -> SelectQuery stage g a)
    -> SelectQuery stage g a
selectFrom f = SelectQuery $ do
    (i, j) <- State.get
    State.put (i + 1, j)
    let ref = RelationRef (RRef i) :: Ref (Entity record)
        alias = RelationAlias i :: RelationAlias (Entity record)
        sref = id :$: ref
    lift . Writer.tell . SelectClauses . return $ From i alias
    unSelectQuery . f ref alias . SelectQuery . return $ sref

selectFromSub
    :: SelectQuery s g b
    -> (FieldsSelector Ref b -> RelationAlias b -> SelectQuery Neutral g b -> SelectQuery stage g a)
    -> SelectQuery stage g a
selectFromSub sub f = SelectQuery $ do
    (i, j) <- State.get
    State.put (i + 1, j)
    let (sref, _) = Writer.runWriter . flip State.evalStateT (0, 0) . unSelectQuery $ sub
        qref = qualifySelectorRef i sref
        alias = RelationAliasSub i qref
    lift . Writer.tell . SelectClauses . return . FromSub i $ sub
    unSelectQuery . f qref alias . SelectQuery . return $ qref

resultAs
    :: FieldsSelector (Sel g) a1
    -> (FieldsSelector Ref a1 -> SelectQuery FieldsSpecified g a1 -> SelectQuery FieldsSpecified g a2)
    -> SelectQuery Neutral g a0
    -> SelectQuery FieldsSpecified g a2
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
    aliasToRef (Star' (RelationAlias i))        = RelationRef (RRef i)
    aliasToRef (Star' (RelationAliasSub i ref)) = RelationRef (RRefSub i ref)
    aliasToRef (Sel' _ (FieldAlias i))          = FieldRef (FRef i)

where_ :: g Bool -> SelectQuery s g a -> SelectQuery s g a
where_ a (SelectQuery q) = SelectQuery $ do
    r <- q
    lift . Writer.tell . SelectClauses . return $ Where a
    return r

groupBy
    :: (Ast g, m ~ ReaderT Aggregated n, Functor n, Member (NodeTypes g) AggregateFunction)
    => AFields (g m) xs
    -> (ARefs (g n) xs -> SelectQuery Aggregated (g n) [PersistValue] -> SelectQuery s (g n) a1)
    -> SelectQuery Neutral (g n) a0
    -> SelectQuery s (g n) a1
groupBy fields cont (SelectQuery pre) = SelectQuery $ do
    _ <- pre
    let removeConstraint = hoist (hoistAst (`runReaderT` Aggregated))
        fields' = runIdentity . HList.htraverse (return . removeConstraint) $ fields
        ref = afieldsToARefs fields'
    lift . Writer.tell . SelectClauses . return $ GroupBy fields'
    unSelectQuery . cont ref . SelectQuery . return $ Raw

orderBy :: g b -> OrderType -> SelectQuery s g a -> SelectQuery s g a
orderBy a t (SelectQuery q) = SelectQuery $ do
    r <- q
    lift . Writer.tell . SelectClauses . return $ OrderBy a t
    return r

limit :: Int64 -> SelectQuery s g a -> SelectQuery s g a
limit a (SelectQuery q) = SelectQuery $ do
    r <- q
    lift . Writer.tell . SelectClauses . return . Limit $ a
    return r

offset :: Int64 -> SelectQuery s g a -> SelectQuery s g a
offset a (SelectQuery q) = SelectQuery $ do
    r <- q
    lift . Writer.tell . SelectClauses . return . Offset $ a
    return r

field :: (Ast g, Monad m, Member (NodeTypes g) Field, PersistField a)
    => FRef a -> g m a
field = mkAst . return . Field

column :: (Ast g, Monad m, Member (NodeTypes g) Field, PersistEntity record)
    => RRef (Entity record) -> EntityField record a -> g m a
column t = mkAst . return . Column t

(.^) :: (Ast g, Monad m, Member (NodeTypes g) Field, PersistEntity record)
    => RRef (Entity record) -> EntityField record a -> g m a
(.^) = column

infixl 9 .^

type AggFunctionType g n a b = (Ast g, Monad n, Member (NodeTypes g) AggregateFunction) => g (ReaderT Aggregated n) a -> g (ReaderT Aggregated n) b

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
hoist' _ (From i a)    = From i a
hoist' f (FromSub i a) = FromSub i (hoist f a)
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
