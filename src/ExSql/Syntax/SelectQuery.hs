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
import qualified Control.Monad.Trans.State.Strict as State (evalStateT, get, put, mapStateT)
import Control.Monad.Trans.Writer.Strict (Writer)
import qualified Control.Monad.Trans.Writer.Strict as Writer (tell, mapWriter, runWriter)
import Data.Int (Int64)
import Data.DList (DList)
import Data.Proxy (Proxy(..))
import Data.Semigroup (Semigroup(..))
import Database.Persist (Entity(..), PersistEntity(..), PersistField(..))
import ExSql.Syntax.Class
import ExSql.Syntax.Internal.Types (RelationAlias(..), FieldAlias(..), Ref(..), Sel(..), SelWithAlias(..), PersistConvert)

newtype SelectQuery c (g :: * -> *) a = SelectQuery
    { unSelectQuery :: StateT (Int, Int) (Writer (SelectClauses g)) (FieldsSelector c Ref a)
    }

data SelectClause (g :: * -> *) where
    Fields :: FieldsSelector c (SelWithAlias g) a -> SelectClause g
    From :: (PersistEntity record) => RelationAlias (Entity record) -> SelectClause g
    FromSub :: Int -> SelectQuery c g a -> SelectClause g
    Where :: g Bool -> SelectClause g
    OrderBy :: g b -> OrderType -> SelectClause g
    Limit :: Int64 -> SelectClause g
    Offset :: Int64 -> SelectClause g
    Initial :: SelectClause g

newtype SelectClauses g = SelectClauses (DList (SelectClause g))
    deriving (Semigroup, Monoid)

instance Hoist (SelectQuery c) where
    hoist f (SelectQuery a) = SelectQuery $ State.mapStateT h a
        where
        h = Writer.mapWriter $ \(x, (SelectClauses w)) -> (x, SelectClauses (fmap (hoist' f) w))

hoist' :: (forall x. m x -> n x) -> SelectClause m -> SelectClause n
hoist' f (Fields a) = Fields (hoist (hoist f) a)
hoist' _ (From a) = From a
hoist' f (FromSub i a) = FromSub i (hoist f a)
hoist' f (Where a) = Where (f a)
hoist' f (OrderBy a t) = OrderBy (f a) t
hoist' _ (Limit a) = Limit a
hoist' _ (Offset a) = Offset a
hoist' _ Initial = Initial

data OrderType = Asc | Desc deriving (Show, Eq)

selectFrom :: forall a c g record. (PersistEntity record) => (Ref (Entity record) -> SelectQuery (Entity record -> Entity record) g (Entity record) -> SelectQuery c g a) -> SelectQuery c g a
selectFrom f = SelectQuery $ do
    (i, j) <- State.get
    State.put (i + 1, j)
    let ref = RelationRef i :: Ref (Entity record)
        alias = RelationAlias i :: RelationAlias (Entity record)
        sref = id :$: ref
    lift . Writer.tell . SelectClauses . return . From $ alias
    unSelectQuery . f ref . SelectQuery . return $ sref

selectFromSub :: SelectQuery c g b -> (FieldsSelector c Ref b -> SelectQuery c g b -> SelectQuery d g a) -> SelectQuery d g a
selectFromSub sub f = SelectQuery $ do
    (i, j) <- State.get
    State.put (i + 1, j)
    let (sref, _) = Writer.runWriter . flip State.evalStateT (0, 0) . unSelectQuery $ sub
        qref = qualifySelectorRef i sref
    lift . Writer.tell . SelectClauses . return . FromSub i $ sub
    unSelectQuery . f qref . SelectQuery . return $ qref

resultAs :: FieldsSelector c1 (Sel g) a1 -> (FieldsSelector c1 Ref a1 -> SelectQuery c1 g a1 -> SelectQuery c2 g a2) -> SelectQuery c0 g a0 -> SelectQuery c2 g a2
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
    aliasToRef (Sel' _ (FieldAlias i)) = FieldRef i

where_ :: g Bool -> SelectQuery c g a -> SelectQuery c g a
where_ a (SelectQuery q) = SelectQuery $ do
    r <- q
    lift . Writer.tell . SelectClauses . return $ Where a
    return r

orderBy :: g b -> OrderType -> SelectQuery c g a -> SelectQuery c g a
orderBy a t (SelectQuery q) = SelectQuery $ do
    r <- q
    lift . Writer.tell . SelectClauses . return $ OrderBy a t
    return r

limit :: Int64 -> SelectQuery c g a -> SelectQuery c g a
limit a (SelectQuery q) = SelectQuery $ do
    r <- q
    lift . Writer.tell . SelectClauses . return . Limit $ a
    return r

offset :: Int64 -> SelectQuery c g a -> SelectQuery c g a
offset a (SelectQuery q) = SelectQuery $ do
    r <- q
    lift . Writer.tell . SelectClauses . return . Offset $ a
    return r

data FieldsSelector c g x where
    (:$:) :: (a -> b) -> g a -> FieldsSelector (a -> b) g b
    (:*:) :: FieldsSelector c g (a -> b) -> g a -> FieldsSelector c g b

infixl 4 :$:, :*:

instance Hoist (FieldsSelector c) where
    hoist f (g :$: a) = g :$: (f a)
    hoist f (s :*: a) = (hoist f s) :*: (f a)

mkSelectorWithAlias :: Int -> FieldsSelector c (Sel g) a -> (FieldsSelector c (SelWithAlias g) a, Int)
mkSelectorWithAlias i (f :$: Star a) = (f :$: Star' a, i)
mkSelectorWithAlias i (f :$: Sel a) = (f :$: Sel' a (FieldAlias i), i + 1)
mkSelectorWithAlias i (s :*: Star a) =
    let (r, next) = mkSelectorWithAlias i s
    in (r :*: Star' a, next)
mkSelectorWithAlias i (s :*: Sel a) =
    let (r, next) = mkSelectorWithAlias i s
    in (r :*: Sel' a (FieldAlias next), next + 1)

qualifySelectorRef :: Int -> FieldsSelector c Ref a -> FieldsSelector c Ref a
qualifySelectorRef tid (f :$: a) = f :$: qualifyRef tid a
qualifySelectorRef tid (s :*: a) = qualifySelectorRef tid s :*: qualifyRef tid a

qualifyRef :: Int -> Ref a -> Ref a
qualifyRef tid (RelationRef _) = RelationRef tid
qualifyRef tid (FieldRef fid) = QualifiedFieldRef tid fid
qualifyRef tid (QualifiedFieldRef _ fid) = QualifiedFieldRef tid fid
