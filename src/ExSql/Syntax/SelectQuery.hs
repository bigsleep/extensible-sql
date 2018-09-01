{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
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
    , countFields
    , field
    , fromId
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
    , fromEntity
    , fromSub
    , join
    , joinEntity
    , joinSub
    , on
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
import Data.DList (DList(..))
import Data.Extensible (Member)
import qualified Data.Extensible.HList as HList (HList(..), htraverse)
import Data.Functor.Identity (Identity(..))
import Data.Int (Int64)
import Data.Proxy (Proxy(..))
import Data.Semigroup (Semigroup(..))
import Data.Text (Text)
import Data.Type.Bool
import Data.Type.Equality
import Database.Persist (Entity(..), PersistEntity(..), PersistField(..),
                         PersistValue)
import qualified Database.Persist.Sql.Util as Persist (entityColumnCount)
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
    FromSubQuery :: Int -> SelectQuery g a -> FieldsSelector Ref a -> From g a

data FromProxy g a where
    FPEntity :: PersistEntity record => proxy (Entity record) -> FromProxy g (Entity record)
    FPSubQuery :: SelectQuery g a -> FromProxy g a

data JoinType =
    InnerJoin |
    CrossJoin |
    LeftJoin |
    RightJoin |
    FullJoin
    deriving (Show, Eq)

newtype JoinProxy (t :: JoinType) g a =
    JoinProxy (FromProxy g a)

data Joins g (xs :: [*]) where
    JNil :: Joins g '[]
    JCons :: JoinProxy t g a -> Joins g xs -> Joins g (JoinProxy t g a ': xs)

data FromJoins g a (xs :: [*]) where
    FromJoins :: FromProxy g a -> Joins g xs -> FromJoins g a xs

type family HandleNullableType (nullable :: Bool) x :: * where
    HandleNullableType 'True (Maybe x) = Maybe x
    HandleNullableType 'True x = Maybe x
    HandleNullableType 'False x = x

type family IsMaybe (a :: *) :: Bool where
    IsMaybe (Maybe a) = 'True
    IsMaybe a = 'False

type family IsNotMaybe (a :: *) :: Bool where
    IsNotMaybe a = Not (IsMaybe a)

type family InList (x :: k) (xs :: [k]) :: Bool where
    InList x (x ': _) = 'True
    InList x (y ': xs) = InList x xs
    InList x '[] = 'False

class RightNullable (xs :: [*]) where
    type RightNullableType xs :: Bool
    rightNullable :: Joins g xs -> Proxy (RightNullableType xs)

instance RightNullable '[] where
    type RightNullableType '[] = 'False
    rightNullable _ = Proxy

instance (RightNullable xs) => RightNullable (JoinProxy x g a ': xs) where
    type RightNullableType (JoinProxy x g a ': xs) = InList x ['RightJoin, 'FullJoin] || RightNullableType xs
    rightNullable _ = Proxy

class HandleFromProxy (n :: Bool) a where
    type HandleFromProxyType n a :: *
    handleFromProxy :: Proxy n -> FromProxy g a -> (From g a -> SelectClause g) -> SelectQueryM g (HandleFromProxyType n a)

instance HandleFromProxy 'True a where
    type HandleFromProxyType 'True a = RefWithAlias a (Maybe a)
    handleFromProxy _ proxy sc = do
        i <- prepareFrom $ return ()
        let from_ = convertFromProxy i proxy
            (sref, RelationAlias j x) = mkRefWithAlias from_
            alias' = RelationAlias j (Nullable x)
        tellSelectClause . sc $ from_
        return (sref, alias')

instance HandleFromProxy 'False a where
    type HandleFromProxyType 'False a = RefWithAlias a a
    handleFromProxy _ proxy sc = do
        i <- prepareFrom $ return ()
        let from_ = convertFromProxy i proxy
        tellSelectClause . sc $ from_
        return . mkRefWithAlias $ from_

type RefWithAlias a b = (FieldsSelector Ref a, RelationAlias b)

class HandleJoins (n :: Bool) (xs :: [*]) where
    type NeedConvert n xs :: Bool
    type HandleJoinsType n xs :: [*]
    type LeftNullableType n xs :: Bool
    needConvert :: Proxy n -> Joins g xs -> Proxy (NeedConvert n xs)
    leftNullable :: Proxy n -> Joins g xs -> Proxy (LeftNullableType n xs)
    handleJoins :: Proxy n -> Joins g xs -> SelectQueryM g (HList.HList Identity (HandleJoinsType n xs))

instance HandleJoins n '[] where
    type NeedConvert n '[] = 'False
    type HandleJoinsType n '[] = '[]
    type LeftNullableType n '[] = n
    needConvert _ _ = Proxy
    handleJoins _ _ = return HList.HNil

instance
    ( HandleJoins (n || InList x ['LeftJoin, 'FullJoin]) xs
    , RightNullable xs
    , HandleFromProxy ((n || x == 'FullJoin || RightNullableType xs) && IsNotMaybe a) a
    ) => HandleJoins n (JoinProxy x g a ': xs) where
    type NeedConvert n (JoinProxy x g a ': xs) = (n || x == 'FullJoin || RightNullableType xs) && IsNotMaybe a
    type HandleJoinsType n (JoinProxy x g a ': xs) = HandleFromProxyType ((n || x == 'FullJoin || RightNullableType xs) && IsNotMaybe a) a ': HandleJoinsType (n || InList x ['LeftJoin, 'FullJoin]) xs
    type LeftNullableType n (JoinProxy x g a ': xs) = n || InList x ['LeftJoin, 'FullJoin]
    needConvert _ _ = Proxy
    handleJoins n js @ (JCons (JoinProxy p) xs) = do
        let n' = needConvert n js
        a <- handleFromProxy n' p Join
        xs <- handleJoins (leftNullable n js) xs
        return (HList.HCons (return a) xs)

handleFromJoins :: (RightNullable xs, HandleFromProxy (RightNullableType xs) a, HandleJoins 'False xs) => FromJoins g a xs -> SelectQueryM g (HList.HList Identity (HandleFromProxyType ('False || RightNullableType xs) a ': HandleJoinsType 'False xs))
handleFromJoins (FromJoins p js) = do
    a <- handleFromProxy (rightNullable js) p From
    xs <- handleJoins (Proxy :: Proxy 'False) js
    return (HList.HCons (return a) xs)

data SelectClause (g :: * -> *) where
    Fields :: FieldsSelector (SelWithAlias g) a -> SelectClause g
    From :: From g a -> SelectClause g
    Join :: From g a -> SelectClause g
    On :: RRef a -> g Bool -> SelectClause g
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
    hoist _ (FromEntity i a)     = FromEntity i a
    hoist f (FromSubQuery i a s) = FromSubQuery i (hoist f a) s

fromId :: From g a -> Int
fromId (FromEntity i _)     = i
fromId (FromSubQuery i _ _) = i

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
    :: FromProxy g b
    -> (FieldsSelector Ref b -> RelationAlias b -> SelectQueryInternal Neutral g b -> SelectQueryInternal s1 g a)
    -> SelectQueryInternal Neutral g x
    -> SelectQueryInternal s1 g a
from proxy f (SelectQueryInternal pre) = SelectQueryInternal $ do
    i <- prepareFrom pre
    let from_ = convertFromProxy i proxy
        (sref, alias) = mkRefWithAlias from_
    tellSelectClause . From $ from_
    unSelectQueryInternal . f sref alias . SelectQueryInternal . return $ sref

fromEntity
    :: (PersistEntity record)
    => (RRef (Entity record) -> RelationAlias (Entity record) -> SelectQueryInternal Neutral g (Entity record) -> SelectQueryInternal s1 g a)
    -> SelectQueryInternal Neutral g x
    -> SelectQueryInternal s1 g a
fromEntity f = from (FPEntity Proxy) g
    where
    g (_ :$: RelationRef rref) = f rref
    g _                        = error "impossible"

fromSub
    :: SelectQuery g b
    -> (FieldsSelector Ref b -> RelationAlias b -> SelectQueryInternal Neutral g b -> SelectQueryInternal s1 g a)
    -> SelectQueryInternal Neutral g x
    -> SelectQueryInternal s1 g a
fromSub = from . FPSubQuery

join
    :: FromProxy g b
    -> (FieldsSelector Ref b -> RelationAlias b -> SelectQueryInternal Neutral g b -> SelectQueryInternal s1 g a)
    -> SelectQueryInternal Neutral g x
    -> SelectQueryInternal s1 g a
join proxy f (SelectQueryInternal pre) = SelectQueryInternal $ do
    i <- prepareFrom pre
    let from_ = convertFromProxy i proxy
        (ref, alias) = mkRefWithAlias from_
    tellSelectClause . Join $ from_
    unSelectQueryInternal . f ref alias . SelectQueryInternal . return $ ref

joinEntity
    :: (PersistEntity record)
    => (RRef (Entity record) -> RelationAlias (Entity record) -> SelectQueryInternal Neutral g (Entity record) -> SelectQueryInternal s1 g a)
    -> SelectQueryInternal Neutral g x
    -> SelectQueryInternal s1 g a
joinEntity f = join (FPEntity Proxy) g
    where
    g (_ :$: RelationRef rref) = f rref
    g _                        = error "impossible"

joinSub
    :: SelectQuery g b
    -> (FieldsSelector Ref b -> RelationAlias b -> SelectQueryInternal Neutral g b -> SelectQueryInternal s1 g a)
    -> SelectQueryInternal Neutral g x
    -> SelectQueryInternal s1 g a
joinSub = join . FPSubQuery

on :: RRef b -> g Bool -> SelectQueryInternal s g a -> SelectQueryInternal s g a
on ref cond (SelectQueryInternal q) = SelectQueryInternal $ do
    r <- q
    lift . Writer.tell . SelectClauses . return $ On ref cond
    return r

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
    aliasToRef (Star' (RelationAlias i ref)) = RelationRef (RRefSub i ref)
    aliasToRef (Sel' _ (FieldAlias i))       = FieldRef (FRef i)

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
hoist' f (Join a) = Join (hoist f a)
hoist' f (On ref cond) = On ref (f cond)
hoist' f (Where a)     = Where (f a)
hoist' f (GroupBy fs)  = GroupBy . runIdentity . HList.htraverse (Identity . hoist f) $ fs
hoist' f (OrderBy a t) = OrderBy (f a) t
hoist' _ (Limit a)     = Limit a
hoist' _ (Offset a)    = Offset a
hoist' _ Initial       = Initial

mkSelectorWithAlias :: Int -> FieldsSelector (Sel g) a -> (FieldsSelector (SelWithAlias g) a, Int)
mkSelectorWithAlias i Raw = (Raw, i)
mkSelectorWithAlias i (Nullable a) =
    let (x, y) = mkSelectorWithAlias i a
    in (Nullable x, y)
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
qualifySelectorRef tid (Nullable a) = Nullable $ qualifySelectorRef tid a
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

convertFromProxy :: Int -> FromProxy g a -> From g a
convertFromProxy i (FPEntity a)   = FromEntity i a
convertFromProxy i (FPSubQuery a) =
    let qref = qualifySelectorRef i . evalSubQueryRef $ a
    in FromSubQuery i a qref

mkRefWithAlias :: From g a -> RefWithAlias a a
mkRefWithAlias (FromEntity i _) =
    let rref = RRef i
        ref = RelationRef rref
        sref = id :$: ref
        alias = RelationAlias i sref
    in (sref, alias)
mkRefWithAlias (FromSubQuery i subq qref) =
    let rref = RRefSub i qref
        ref = RelationRef rref
        alias = RelationAlias i qref
    in (qref, alias)

countFields :: Int -> FieldsSelector Ref a -> Int
countFields i Raw = i
countFields i (Nullable a) = countFields i a
countFields i (_ :$: RelationRef rref) = countRRefFields i rref
countFields _ (_ :$: FieldRef {}) = 1
countFields i (a :*: RelationRef rref) = countFields i a + countRRefFields i rref
countFields i (a :*: FieldRef {}) = countFields i a + 1

countRRefFields :: Int -> RRef a -> Int
countRRefFields _ a @ RRef {} = colCount
    where
    def = entityDef . fmap entityVal . toProxy $ a
    colCount = Persist.entityColumnCount def
countRRefFields i (RRefSub _ a) = countFields i a

toProxy :: f a -> Proxy a
toProxy _ = Proxy
