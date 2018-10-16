{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
module ExSql.Syntax.SelectQuery
    ( (.^)
    , AFields
    , ARef(..)
    , ARefs
    , AggregateFunction(..)
    , Field(..)
    , FieldClause(..)
    , FieldsSelector(..)
    , From(..)
    , FromProxy(..)
    , OrderType(..)
    , SelectQuery(..)
    , SelectQueryInternal(..)
    , SelectQueryM
    , afield
    , aggResultAs
    , avg
    , column
    , convertFromProxy
    , count
    , countFields
    , field
    , from
    , fromEntity
    , fromId
    , fromSub
    , groupBy
    , join
    , joinEntity
    , joinSub
    , limit
    , max
    , min
    , mkSelRefAlias
    , offset
    , on
    , orderBy
    , prepareFrom
    , resultAs
    , select
    , selectAgg
    , selectAgg_
    , selectInternal
    , select_
    , stddev
    , sum
    , tellSelectClause
    , variance
    , where_
    ) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import qualified Control.Monad.Trans.State.Strict as State (get, put)
import qualified Control.Monad.Trans.Writer.Strict as Writer (tell)
import Data.Extensible (Member)
import qualified Data.Extensible.HList as HList (HList(..), htraverse)
import Data.Functor.Identity (Identity(..))
import Data.Int (Int64)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import Database.Persist (Entity(..), PersistEntity(..), PersistField(..),
                         PersistValue)
import ExSql.Syntax.Class
import ExSql.Syntax.Internal
import ExSql.Syntax.Internal.Types
import Prelude hiding (max, min, sum)

data AggregateFunction g a where
    AggFunction :: Text -> g a -> AggregateFunction g a
    AggField :: ARef g a -> AggregateFunction g a
    Count :: g a -> AggregateFunction g Int64

instance Hoist AggregateFunction where
    hoist f (AggFunction name e) = AggFunction name (f e)
    hoist f (AggField ref)       = AggField (hoist f ref)
    hoist f (Count e)            = Count (f e)

type ARefs g xs = HList.HList (ARef g) xs

select_ :: Selectable t => (SelectQueryInternal 'Neutral FieldsSelector g [PersistValue] -> SelectQueryInternal 'Neutral t g a) -> SelectQuery t g a
select_ = SelectQuery . selectInternal

select :: Selectable t => (SelectQueryInternal 'Neutral FieldsSelector g [PersistValue] -> SelectQueryInternal 'FieldsSpecified t g a) -> SelectQuery t g a
select = SelectQuery . selectInternal

selectAgg_ :: Selectable t => (SelectQueryInternal 'Neutral FieldsSelector g [PersistValue] -> SelectQueryInternal 'Aggregated t g a) -> SelectQuery t g a
selectAgg_ = SelectQuery . selectInternal

selectAgg :: Selectable t => (SelectQueryInternal 'Neutral FieldsSelector g [PersistValue] -> SelectQueryInternal 'AggFieldsSpecified t g a) -> SelectQuery t g a
selectAgg = SelectQuery . selectInternal

selectInternal :: (SelectQueryInternal s0 FieldsSelector g [PersistValue] -> SelectQueryInternal s1 t g a) -> SelectQueryInternal s1 t g a
selectInternal f = f . SelectQueryInternal . return $ Raw

from
    :: (Selectable t)
    => FromProxy c t g b
    -> (SelectRefType t b -> RRef b -> RelationAlias c -> SelectQueryInternal 'Neutral t g b -> SelectQueryInternal s1 t1 g a)
    -> SelectQueryInternal 'Neutral t0 g x
    -> SelectQueryInternal s1 t1 g a
from proxy f (SelectQueryInternal pre) = SelectQueryInternal $ do
    i <- prepareFrom pre
    let from_ = convertFromProxy i proxy
        rref = RRef i
        (sel, sref, alias) = mkSelRefAlias from_
    tellSelectClause . From $ from_
    unSelectQueryInternal . f sref rref alias . SelectQueryInternal . return $ sel

fromEntity
    :: (PersistEntity record)
    => (RRef (Entity record) -> RelationAlias (Entity record) -> SelectQueryInternal 'Neutral FieldsSelector g (Entity record) -> SelectQueryInternal s1 t1 g a)
    -> SelectQueryInternal 'Neutral t0 g x
    -> SelectQueryInternal s1 t1 g a
fromEntity f =
    from (FPEntity Proxy) (const f)

fromSub
    :: SelectQuery FieldsSelector g b
    -> (FieldsSelector Ref b -> RelationAlias b -> SelectQueryInternal 'Neutral FieldsSelector g b -> SelectQueryInternal s1 t1 g a)
    -> SelectQueryInternal 'Neutral t0 g x
    -> SelectQueryInternal s1 t1 g a
fromSub q f =
    from (FPSubQuery q) g
    where
    g a r = f (qualifySelectorRef (rrefId r) a)

join
    :: (Selectable t)
    => FromProxy c t g b
    -> (SelectRefType t b -> RRef b -> RelationAlias c -> SelectQueryInternal 'Neutral t g b -> SelectQueryInternal s1 t1 g a)
    -> SelectQueryInternal 'Neutral t0 g x
    -> SelectQueryInternal s1 t1 g a
join proxy f (SelectQueryInternal pre) = SelectQueryInternal $ do
    i <- prepareFrom pre
    let from_ = convertFromProxy i proxy
        rref = RRef i
        (sel, ref, alias) = mkSelRefAlias from_
    tellSelectClause . Join $ from_
    unSelectQueryInternal . f ref rref alias . SelectQueryInternal . return $ sel

joinEntity
    :: (PersistEntity record)
    => (RRef (Entity record) -> RelationAlias (Entity record) -> SelectQueryInternal 'Neutral FieldsSelector g (Entity record) -> SelectQueryInternal s1 t1 g a)
    -> SelectQueryInternal 'Neutral t0 g x
    -> SelectQueryInternal s1 t1 g a
joinEntity f = join (FPEntity Proxy) (const f)

joinSub
    :: SelectQuery FieldsSelector g b
    -> (FieldsSelector Ref b -> RelationAlias b -> SelectQueryInternal 'Neutral FieldsSelector g b -> SelectQueryInternal s1 t1 g a)
    -> SelectQueryInternal 'Neutral t0 g x
    -> SelectQueryInternal s1 t1 g a
joinSub q f = join (FPSubQuery q) g
    where
    g a r = f (qualifySelectorRef (rrefId r) a)

on :: RRef b -> g Bool -> SelectQueryInternal s t g a -> SelectQueryInternal s t g a
on ref cond (SelectQueryInternal q) = SelectQueryInternal $ do
    r <- q
    lift . Writer.tell . SelectClauses . return $ On ref cond
    return r

resultAs
    :: (Selectable t, Ast g, expr ~ g Identity)
    => t (Sel expr) a1
    -> (SelectRefType t a1 -> SelectQueryInternal 'FieldsSpecified t expr a1 -> SelectQueryInternal 'FieldsSpecified t expr a2)
    -> SelectQueryInternal 'Neutral t0 expr a0
    -> SelectQueryInternal 'FieldsSpecified t expr a2
resultAs = resultAsInternal

aggResultAs
    :: (Selectable t, Ast g, expr0 ~ g (ReaderT AggregatedE Identity), expr1 ~ g Identity)
    => t (Sel expr0) a1
    -> (SelectRefType t a1 -> SelectQueryInternal 'AggFieldsSpecified t expr1 a1 -> SelectQueryInternal 'AggFieldsSpecified t expr1 a2)
    -> SelectQueryInternal 'Aggregated t0 expr1 a0
    -> SelectQueryInternal 'AggFieldsSpecified t expr1 a2
aggResultAs selector = resultAsInternal selector'
    where
    selector' = hoist (hoist (hoistAst (`runReaderT` AggregatedE))) selector

resultAsInternal
    :: Selectable t
    => t (Sel g) a1
    -> (SelectRefType t a1 -> SelectQueryInternal stage1 t g a1 -> SelectQueryInternal stage1 t g a2)
    -> SelectQueryInternal stage0 t0 g a0
    -> SelectQueryInternal stage1 t g a2
resultAsInternal selector cont (SelectQueryInternal pre) = SelectQueryInternal $ do
    _ <- pre
    let (!ref, !fs) = mkRefAndFieldClauses selector
    lift . Writer.tell . SelectClauses . return . Fields $ fs
    unSelectQueryInternal . cont ref . SelectQueryInternal . return $ selector

where_ :: g Bool -> SelectQueryInternal s t g a -> SelectQueryInternal s t g a
where_ a (SelectQueryInternal q) = SelectQueryInternal $ do
    r <- q
    lift . Writer.tell . SelectClauses . return $ Where a
    return r

groupBy
    :: (Ast g, Functor m, Member (NodeTypes g) AggregateFunction)
    => AFields (g m) xs
    -> (ARefs (g m) xs -> SelectQueryInternal 'Aggregated FieldsSelector (g m) [PersistValue] -> SelectQueryInternal s t (g m) a1)
    -> SelectQueryInternal 'Neutral t (g m) a0
    -> SelectQueryInternal s t (g m) a1
groupBy fields cont pre = SelectQueryInternal $ do
    _ <- unSelectQueryInternal pre
    let ref = afieldsToARefs fields
    lift . Writer.tell . SelectClauses . return $ GroupBy fields
    unSelectQueryInternal . cont ref . SelectQueryInternal . return $ Raw

orderBy :: g b -> OrderType -> SelectQueryInternal s t g a -> SelectQueryInternal s t g a
orderBy a t (SelectQueryInternal q) = SelectQueryInternal $ do
    r <- q
    lift . Writer.tell . SelectClauses . return $ OrderBy a t
    return r

limit :: Int64 -> SelectQueryInternal s t g a -> SelectQueryInternal s t g a
limit a (SelectQueryInternal q) = SelectQueryInternal $ do
    r <- q
    lift . Writer.tell . SelectClauses . return . Limit $ a
    return r

offset :: Int64 -> SelectQueryInternal s t g a -> SelectQueryInternal s t g a
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

type AggFunctionType g n a b = (Ast g, Monad n, Member (NodeTypes g) AggregateFunction) => g (ReaderT AggregatedE n) a -> g (ReaderT AggregatedE n) b

afield :: (Ast g, Monad n, Member (NodeTypes g) AggregateFunction) => ARef (g n) a -> g (ReaderT AggregatedE n) a
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

qualifySelectorRef :: Int -> FieldsSelector Ref a -> FieldsSelector Ref a
qualifySelectorRef _ Raw = Raw
qualifySelectorRef tid (f :$: a) = f :$: qualifyRef tid a
qualifySelectorRef tid (s :*: a) = qualifySelectorRef tid s :*: qualifyRef tid a

qualifyRef :: Int -> Ref a -> Ref a
qualifyRef _ (RelationRef ref)         = RelationRef ref
qualifyRef tid (FieldRef (FRef fid))   = FieldRef (QRef tid fid)
qualifyRef tid (FieldRef (QRef _ fid)) = FieldRef (QRef tid fid)

afieldsToARefs :: AFields g xs -> ARefs g xs
afieldsToARefs = runIdentity . HList.htraverse f
    where
    f = Identity . ARef

prepareFrom :: SelectQueryM g b -> SelectQueryM g Int
prepareFrom pre = do
    _ <- pre
    (i, j) <- State.get
    State.put (i + 1, j)
    return i

tellSelectClause :: SelectClause g -> SelectQueryM g ()
tellSelectClause = lift . Writer.tell . SelectClauses . return

convertFromProxy :: (Selectable t) => Int -> FromProxy x t g a -> From x t g a
convertFromProxy i (FPEntity _)   = FromEntity (RelationAlias i)
convertFromProxy i (FPSubQuery a) =
    let sel = evalSubQuerySel a
        alias = RelationAliasSub i (`countFields` sel) (mkPersistConvert sel)
    in FromSubQuery alias a

mkSelRefAlias :: (Selectable t) => From x t g a -> (t (Sel g) a, SelectRefType t a, RelationAlias x)
mkSelRefAlias a =
    let sel = mkRelationSel a
        (ref, _) = mkRefAndFieldClauses sel
        alias = getRelationAlias a
    in (sel, ref, alias)
