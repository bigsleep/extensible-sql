{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module ExSql.Printer.SelectQuery
    ( Clause(..)
    , OrderByClause(..)
    , SelectClauses(..)
    , printField
    , printAggregateFunction
    , renderSelect
    , printRelationAlias
    , printFieldAlias
    , printSelect
    , printSelectClauses
    ) where

import Control.Monad (MonadPlus(..))
import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.State.Strict as State (evalStateT, get,
                                                            mapStateT, put)
import qualified Control.Monad.Trans.Writer.Strict as Writer (runWriter)
import Data.DList (DList)
import qualified Data.DList as DList
import Data.Extensible (Membership)
import qualified Data.Extensible.HList as HList (hfoldrWithIndex)
import Data.Int (Int64)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.List (intersperse, uncons)
import Data.Maybe (maybe)
import Data.Proxy (Proxy(..))
import Data.Semigroup (Semigroup(..))
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Text.Lazy.Builder.Int as TLB
import qualified Database.Persist as Persist (DBName(..), Entity(..),
                                              EntityDef(..), PersistEntity(..),
                                              PersistField(..))
import qualified Database.Persist.Sql as Persist (fieldDBName)
import qualified Database.Persist.Sql.Util as Persist (entityColumnCount,
                                                       parseEntityValues)
import Safe.Exact (splitAtExactMay)

import ExSql.Printer.Common
import ExSql.Printer.Types
import ExSql.Syntax.Internal
import ExSql.Syntax.Internal.Types
import ExSql.Syntax.Relativity (Associativity(..), Precedence(..),
                                Relativity(..))
import ExSql.Syntax.SelectQuery (ARef(..), AggregateFunction(..), Field(..),
                                 FieldsSelector(..), From(..), OrderType(..),
                                 SelectQuery(..), SelectQueryInternal(..))
import qualified ExSql.Syntax.SelectQuery as Syntax

newtype Clause = Clause (DList StatementBuilder)
    deriving (Show, Semigroup, Monoid, Eq)

newtype JoinClause = JoinClause (IntMap StatementBuilder)
    deriving (Show, Semigroup, Monoid, Eq)

newtype OnClause = OnClause (IntMap StatementBuilder)
    deriving (Show, Semigroup, Monoid, Eq)

newtype OrderByClause = OrderByClause (DList (StatementBuilder, OrderType))
    deriving (Show, Semigroup, Monoid, Eq)

data LimitClause = LimitClause (Maybe Int64) (Maybe Int64)
    deriving (Show, Eq)

data SelectClauses = SelectClauses
    { scField   :: !Clause
    , scFrom    :: !Clause
    , scJoin    :: !JoinClause
    , scOn      :: !OnClause
    , scWhere   :: !Clause
    , scGroupBy :: !Clause
    , scOrderBy :: !OrderByClause
    , scLimit   :: !LimitClause
    } deriving (Show, Eq)

instance Semigroup SelectClauses where
    (<>) (SelectClauses field0 from0 join0 on0 where0 groupBy0 orderBy0 (LimitClause offset0 limit0)) (SelectClauses field1 from1 join1 on1 where1 groupBy1 orderBy1 (LimitClause offset1 limit1)) =
        SelectClauses (field0 <> field1) (from0 <> from1) (join0 <> join1) (on0 <> on1) (where0 <> where1) (groupBy0 <> groupBy1) (orderBy0 <> orderBy1) (LimitClause (offset0 `mplus` offset1) (limit0 `mplus` limit1))

instance Monoid SelectClauses where
    mempty = SelectClauses mempty mempty mempty mempty mempty mempty mempty (LimitClause Nothing Nothing)
    mappend = (<>)

printSelect :: ExprPrinterType g -> SelectQuery g a -> StatementBuilder
printSelect p query =
    let (_, sc) = renderSelect p query
    in printSelectClauses sc

printField :: ExprPrinterType g -> PrinterType g Field a
printField _ l r (Field (RelationRef (RRef tid))) = printQF l r (printRelationAlias tid) (printFieldAlias 0)
printField _ _ _ (Field (FieldRef (FRef fid))) = StatementBuilder (printFieldAlias fid, mempty)
printField _ l r (Field (FieldRef (QRef tid fid))) = printQF l r (printRelationAlias tid) (printFieldAlias fid)
printField _ l r (Column rref col) = printQF l r (printRelationAlias tid) (TLB.fromText columnName)
    where
    tid = rrefId rref
    columnName = Persist.unDBName . Persist.fieldDBName $ col

printQF
    :: Maybe Relativity
    -> Maybe Relativity
    -> TLB.Builder
    -> TLB.Builder
    -> StatementBuilder
printQF l r a b =
    StatementBuilder (handleBracket l c r x, mempty)
    where
    c = Relativity (Precedence 3) LeftToRight
    x = a `mappend` TLB.singleton '.' `mappend` b

printAggregateFunction :: ExprPrinterType g -> PrinterType g AggregateFunction a
printAggregateFunction p _ _ (AggFunction f a) = printFun f [p Nothing Nothing a]
printAggregateFunction p l r (AggField (ARef field)) = printField p l r field
printAggregateFunction p _ _ (Count a) = printFun "COUNT" [p Nothing Nothing a]

printSelectClauses :: SelectClauses -> StatementBuilder
printSelectClauses (SelectClauses field from join on where_ groupBy orderBy limit) =
    StatementBuilder ("SELECT ", mempty)
    <> printFieldClause field
    <> printFromClause from
    <> printJoinOnClause join on
    <> printWhereClause where_
    <> printGroupByClause groupBy
    <> printOrderByClause orderBy
    <> printLimitClause limit

printFieldClause :: Clause -> StatementBuilder
printFieldClause (Clause DList.Nil) = StatementBuilder (" * ", mempty)
printFieldClause (Clause xs) = StatementBuilder (t, mconcat ps)
    where
    (ts, ps) = unzip . map unStatementBuilder . DList.toList $ xs
    t = mconcat . intersperse (TLB.fromText ", ") $ ts

printFromClause :: Clause -> StatementBuilder
printFromClause (Clause xs) = StatementBuilder (t, mconcat ps)
    where
    (ts, ps) = unzip . map unStatementBuilder . DList.toList $ xs
    t = TLB.fromText " FROM " <> mconcat (intersperse (TLB.fromText ", ") ts)

printJoinOnClause :: JoinClause -> OnClause -> StatementBuilder
printJoinOnClause (JoinClause js) (OnClause os) = StatementBuilder (t, p)
    where
    merged = IntMap.mergeWithKey
        (\_ a b -> Just (a, Just b))
        (IntMap.map $ flip (,) Nothing)
        (const IntMap.empty) js os
    f (StatementBuilder (a0, p0), Just (StatementBuilder (a1, p1))) =
        (TLB.fromText " JOIN " <> a0 <> " ON " <> a1, p0 <> p1)
    f (StatementBuilder (a0, p0), Nothing) =
        (TLB.fromText " JOIN " <> a0, p0)
    (ts, ps) = unzip . map f . IntMap.elems $ merged
    t = mconcat ts
    p = mconcat ps

printWhereClause :: Clause -> StatementBuilder
printWhereClause (Clause DList.Nil) = mempty
printWhereClause (Clause xs) = StatementBuilder (t, mconcat ps)
    where
    (ts, ps) = unzip . map unStatementBuilder . DList.toList $ xs
    t = TLB.fromText " WHERE "
        <> (mconcat . intersperse (TLB.fromText " AND ") $ ts)

printGroupByClause :: Clause -> StatementBuilder
printGroupByClause (Clause DList.Nil) = mempty
printGroupByClause (Clause xs) = StatementBuilder (t, mconcat ps)
    where
    (ts, ps) = unzip . map unStatementBuilder . DList.toList $ xs
    t = TLB.fromText " GROUP BY "
        <> (addBracket . mconcat . intersperse (TLB.fromText ", ") $ ts)

printOrderByClause :: OrderByClause -> StatementBuilder
printOrderByClause (OrderByClause DList.Nil) = mempty
printOrderByClause (OrderByClause xs) = StatementBuilder (tlb, mconcat ps)
    where
    ys = DList.toList xs
    ps = map (snd . unStatementBuilder . fst) ys
    ts = map (\(StatementBuilder (t, _), order) -> t <> TLB.singleton ' ' <> TLB.fromText (showOrder order)) ys
    tlb = TLB.fromText " ORDER BY " <> (mconcat . intersperse (TLB.fromText ", ") $ ts)
    showOrder Asc  = "ASC"
    showOrder Desc = "DESC"

printLimitClause :: LimitClause -> StatementBuilder
printLimitClause (LimitClause Nothing Nothing) = mempty
printLimitClause (LimitClause (Just offset) Nothing) = StatementBuilder (TLB.fromText " OFFSET " <> TLB.decimal offset, mempty)
printLimitClause (LimitClause Nothing (Just limit)) = StatementBuilder (TLB.fromText " LIMIT " <> TLB.decimal limit, mempty)
printLimitClause (LimitClause (Just offset) (Just limit)) = StatementBuilder (TLB.fromText " OFFSET " <> TLB.decimal offset <> TLB.fromText " LIMIT " <> TLB.decimal limit, mempty)

renderSelect :: ExprPrinterType g -> SelectQuery g a -> (PersistConvert a, SelectClauses)
renderSelect p (SelectQuery query) = (convert, clauses)
    where
    (sref, Syntax.SelectClauses sclauses) = Writer.runWriter . flip State.evalStateT (0, 0) . unSelectQueryInternal $ query
    convert = mkPersistConvert sref
    clauses = foldMap (renderSelectClause p) sclauses

renderSelectClause :: ExprPrinterType g -> Syntax.SelectClause g -> SelectClauses
renderSelectClause p (Syntax.Fields fs) = mempty { scField = renderSelectorFields p fs }
renderSelectClause p (Syntax.From a) = mempty { scFrom = renderFrom p a }
renderSelectClause p (Syntax.Join a) = mempty { scJoin = renderJoin p a }
renderSelectClause p (Syntax.On a cond) = mempty { scOn = renderOn p a cond }
renderSelectClause p (Syntax.Where w) = mempty { scWhere = Clause . return . p Nothing Nothing $ w }
renderSelectClause p (Syntax.GroupBy fs) = mempty { scGroupBy = renderAFields p fs }
renderSelectClause p (Syntax.OrderBy a t) = mempty { scOrderBy = OrderByClause . return $ (p Nothing Nothing a, t) }
renderSelectClause _ (Syntax.Limit limit) = mempty { scLimit = LimitClause Nothing (Just limit) }
renderSelectClause _ (Syntax.Offset offset) = mempty { scLimit = LimitClause (Just offset) Nothing }
renderSelectClause _ Syntax.Initial = mempty

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left a)  = Left (f a)
mapLeft _ (Right c) = Right c

toProxy :: f a -> Proxy a
toProxy _ = Proxy

mkPersistConvert :: FieldsSelector (Sel g) a -> PersistConvert a
mkPersistConvert Raw =  do
    xs <- State.get
    State.put mempty
    return xs
mkPersistConvert (Nullable a) = do
    xs <- State.get
    let l = length xs
        c = Syntax.countFields l a
    State.mapStateT (handleNullable xs c) (mkPersistConvert a)
    where
    handleNullable _ _ (Right (x, s)) = Right (Just x, s)
    handleNullable vs c (Left HitNullValue) = do
        (_, rest) <- maybe (Left . ConvertError $ "not enough input values") return (splitAtExactMay c vs)
        return (Nothing, rest)
    handleNullable _ _ (Left (ConvertError e)) = Left (ConvertError e)
mkPersistConvert (f :$: Star a @ RelationAlias {}) = f <$> mkPersistConvertEntity a
mkPersistConvert (f :$: Star (RelationAliasSub _ s)) = f <$> mkPersistConvert s
mkPersistConvert (f :$: Sel {}) = mkPersistConvertInternal f
mkPersistConvert (s :*: Star a @ RelationAlias {}) = mkPersistConvert s <*> mkPersistConvertEntity a
mkPersistConvert (s0 :*: Star (RelationAliasSub _ s1)) = mkPersistConvert s0 <*> mkPersistConvert s1
mkPersistConvert (s :*: Sel {}) = mkPersistConvert s >>= mkPersistConvertInternal

mkPersistConvertEntity :: (Persist.PersistEntity a) => RelationAlias (Persist.Entity a) -> PersistConvert (Persist.Entity a)
mkPersistConvertEntity a = do
    let def = Persist.entityDef . fmap Persist.entityVal . toProxy $ a
        colNum = Persist.entityColumnCount def
    xs <- State.get
    (vals, rest) <- maybe (lift . Left . ConvertError $ "not enough input values") return (splitAtExactMay colNum xs)
    State.put rest
    lift . mapLeft ConvertError $ Persist.parseEntityValues def vals

mkPersistConvertInternal :: (Persist.PersistField t) => (t -> a) -> PersistConvert a
mkPersistConvertInternal f = do
    xs <- State.get
    (val, rest) <- maybe (lift . Left . ConvertError $ "not enough input values") return (uncons xs)
    State.put rest
    r <- lift . mapLeft ConvertError . Persist.fromPersistValue $ val
    return (f r)

renderFrom :: ExprPrinterType g -> From g a -> Clause
renderFrom p = Clause . return . renderFrom' p

renderFrom' :: ExprPrinterType g -> From g a -> StatementBuilder
renderFrom' _ (FromEntity eid ref) =
    let tableName = Persist.unDBName . Persist.entityDB . Persist.entityDef . fmap Persist.entityVal . toProxy $ ref
        alias = printRelationAlias eid
        a = TLB.fromText tableName <> TLB.fromText " AS " <> alias
    in StatementBuilder (a, mempty)

renderFrom' p (FromSubQuery tid query _) =
    let alias = printRelationAlias tid
        StatementBuilder (t, ps) = printSelect p query
        a = addBracket t <> TLB.fromText " AS " <> alias
    in StatementBuilder (a, ps)

renderJoin :: ExprPrinterType g -> From g a -> JoinClause
renderJoin p a = JoinClause . IntMap.singleton (Syntax.fromId a) $  renderFrom' p a

renderOn :: ExprPrinterType g -> RRef a -> g Bool -> OnClause
renderOn p a cond = OnClause . IntMap.singleton (rrefId a) $  p Nothing Nothing cond

renderSelectorFields :: ExprPrinterType g -> [Syntax.FieldClause g] -> Clause
renderSelectorFields p = mconcat . map (renderFieldClause p)

renderFieldClause :: ExprPrinterType g -> Syntax.FieldClause g -> Clause
renderFieldClause _ (Syntax.FieldClause (Star' alias)) = renderFieldWildcard alias
renderFieldClause p (Syntax.FieldClause (Sel' a alias)) = renderFieldAlias (p Nothing Nothing a) alias

renderFieldWildcard :: RelationAlias a -> Clause
renderFieldWildcard (RelationAlias eid)      = renderFieldWildcardInternal eid
renderFieldWildcard (RelationAliasSub eid _) = renderFieldWildcardInternal eid

renderFieldWildcardInternal :: Int -> Clause
renderFieldWildcardInternal eid =
    let alias = printRelationAlias eid
        a = alias <> TLB.fromText ".*"
    in Clause . return . StatementBuilder $ (a, mempty)

renderFieldAlias :: StatementBuilder -> FieldAlias a -> Clause
renderFieldAlias a (FieldAlias fid) =
    let alias = printFieldAlias fid
        StatementBuilder (e, ps) = a
        eas = e <> TLB.fromText " AS " <> alias
    in Clause . return . StatementBuilder $ (eas, ps)

renderAFields :: forall g xs. ExprPrinterType g -> Syntax.AFields g xs -> Clause
renderAFields p = HList.hfoldrWithIndex f mempty
    where
    f :: Membership xs x -> Field g x -> Clause -> Clause
    f _ a r = r `mappend` (Clause . return $ printField p Nothing Nothing a)
