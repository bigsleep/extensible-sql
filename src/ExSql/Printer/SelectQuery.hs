{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
module ExSql.Printer.SelectQuery
    ( Clause(..)
    , OrderByClause(..)
    , SelectClauses(..)
    , printField
    , renderSelect
    , printFromAlias
    , printFieldAlias
    , printSelect
    , printSelectClauses
    ) where

import Control.Monad (MonadPlus(..))
import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.State.Strict as State (evalStateT, get,
                                                            put)
import qualified Control.Monad.Trans.Writer.Strict as Writer (runWriter)
import Data.DList (DList)
import qualified Data.DList as DList
import Data.Functor.Identity (Identity(..))
import Data.Int (Int64)
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
import ExSql.Syntax.Class
import ExSql.Syntax.Internal.Types
import ExSql.Syntax.Relativity (Associativity(..), Precedence(..),
                                Relativity(..))
import ExSql.Syntax.SelectQuery (Field(..), FieldsSelector(..), OrderType(..),
                                 SelectQuery(..))
import qualified ExSql.Syntax.SelectQuery as Syntax

newtype Clause = Clause (DList StatementBuilder)
    deriving (Show, Semigroup, Monoid, Eq)

newtype OrderByClause = OrderByClause (DList (StatementBuilder, OrderType))
    deriving (Show, Semigroup, Monoid, Eq)

data LimitClause = LimitClause (Maybe Int64) (Maybe Int64)
    deriving (Show, Eq)

data SelectClauses = SelectClauses
    { scField   :: !Clause
    , scFrom    :: !Clause
    , scWhere   :: !Clause
    , scOrderBy :: !OrderByClause
    , scLimit   :: !LimitClause
    } deriving (Show, Eq)

instance Semigroup SelectClauses where
    (<>) (SelectClauses field0 from0 where0 orderBy0 (LimitClause offset0 limit0)) (SelectClauses field1 from1 where1 orderBy1 (LimitClause offset1 limit1)) =
        SelectClauses (field0 <> field1) (from0 <> from1) (where0 <> where1) (orderBy0 <> orderBy1) (LimitClause (offset0 `mplus` offset1) (limit0 `mplus` limit1))

instance Monoid SelectClauses where
    mempty = SelectClauses mempty mempty mempty mempty (LimitClause Nothing Nothing)
    mappend = (<>)

printSelect :: ExprPrinterType g -> SelectQuery s g a -> StatementBuilder
printSelect p query =
    let (_, sc) = renderSelect p query
    in printSelectClauses sc

printField :: ExprPrinterType (Expr xs Identity) -> PrinterType (Expr xs Identity) Field a
printField _ _ _ (Field (FRef fid)) = StatementBuilder (printFieldAlias fid, mempty)
printField _ l r (Field (QRef tid fid)) =
    StatementBuilder (handleBracket l c r x, mempty)
    where
    c = Relativity (Precedence 3) LeftToRight
    x = printFromAlias tid `mappend` printFieldAlias fid
printField _ l r (Column rref col) =
    StatementBuilder (handleBracket l c r x, mempty)
    where
    c = Relativity (Precedence 3) LeftToRight
    columnName = Persist.unDBName . Persist.fieldDBName $ col
    x = printFromAlias (getTid rref)
        `mappend` TLB.singleton '.'
        `mappend` TLB.fromText columnName
    getTid (RRef tid)      = tid
    getTid (RRefSub tid _) = tid

printSelectClauses :: SelectClauses -> StatementBuilder
printSelectClauses (SelectClauses field from where_ orderBy limit) =
    StatementBuilder ("SELECT ", mempty)
    <> printFieldClause field
    <> printFromClause from
    <> printWhereClause where_
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

printWhereClause :: Clause -> StatementBuilder
printWhereClause (Clause DList.Nil) = mempty
printWhereClause (Clause xs) = StatementBuilder (t, mconcat ps)
    where
    (ts, ps) = unzip . map unStatementBuilder . DList.toList $ xs
    t = TLB.fromText " WHERE "
        <> (mconcat . intersperse (TLB.fromText " AND ") $ ts)

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

renderSelect :: ExprPrinterType g -> SelectQuery s g a -> (PersistConvert a, SelectClauses)
renderSelect p query = (convert, clauses)
    where
    (sref, Syntax.SelectClauses sclauses) = Writer.runWriter . flip State.evalStateT (0, 0) . unSelectQuery $ query
    convert = mkPersistConvert sref
    clauses = foldMap (renderSelectClause p) sclauses

renderSelectClause :: ExprPrinterType g -> Syntax.SelectClause g -> SelectClauses
renderSelectClause p (Syntax.Fields fs) = mempty { scField = renderSelectorFields p fs }
renderSelectClause _ (Syntax.From i a) = mempty { scFrom = renderFrom i a }
renderSelectClause p (Syntax.FromSub i q) = mempty { scFrom = renderFromSub p i q }
renderSelectClause p (Syntax.Where w) = mempty { scWhere = Clause . return . p Nothing Nothing $ w }
renderSelectClause p (Syntax.OrderBy a t) = mempty { scOrderBy = OrderByClause . return $ (p Nothing Nothing a, t) }
renderSelectClause _ (Syntax.Limit limit) = mempty { scLimit = LimitClause Nothing (Just limit) }
renderSelectClause _ (Syntax.Offset offset) = mempty { scLimit = LimitClause (Just offset) Nothing }
renderSelectClause _ Syntax.Initial = mempty

toProxy :: f a -> Proxy a
toProxy _ = Proxy

mkPersistConvert :: FieldsSelector Ref a -> PersistConvert a
mkPersistConvert (f :$: RelationRef a @ RRef {}) = f <$> mkPersistConvertEntity a
mkPersistConvert (f :$: RelationRef (RRefSub _ ref)) = f <$> mkPersistConvert ref
mkPersistConvert (f :$: FieldRef {}) = mkPersistConvertInternal f
mkPersistConvert (s :*: RelationRef a @ RRef {}) = mkPersistConvert s <*> mkPersistConvertEntity a
mkPersistConvert (s :*: RelationRef (RRefSub _ ref)) = mkPersistConvert s <*> mkPersistConvert ref
mkPersistConvert (s :*: FieldRef {}) = mkPersistConvert s >>= mkPersistConvertInternal

mkPersistConvertEntity :: (Persist.PersistEntity a) => RRef (Persist.Entity a) -> PersistConvert (Persist.Entity a)
mkPersistConvertEntity a = do
    let def = Persist.entityDef . fmap Persist.entityVal . toProxy $ a
        colNum = Persist.entityColumnCount def
    xs <- State.get
    (vals, rest) <- maybe (lift . Left $ "not enough input values") return (splitAtExactMay colNum xs)
    State.put rest
    lift $ Persist.parseEntityValues def vals

mkPersistConvertInternal :: (Persist.PersistField t) => (t -> a) -> PersistConvert a
mkPersistConvertInternal f = do
    xs <- State.get
    (val, rest) <- maybe (lift . Left $ "not enough input values") return (uncons xs)
    State.put rest
    r <- lift . Persist.fromPersistValue $ val
    return (f r)

renderFrom :: (Persist.PersistEntity record) => Int -> proxy (Persist.Entity record) -> Clause
renderFrom eid ref =
    let tableName = Persist.unDBName . Persist.entityDB . Persist.entityDef . fmap Persist.entityVal . toProxy $ ref
        alias = printFromAlias eid
        a = TLB.fromText tableName <> TLB.fromText " AS " <> alias
    in Clause . return . StatementBuilder $ (a, mempty)

renderFromSub :: ExprPrinterType g -> Int -> SelectQuery s g a -> Clause
renderFromSub p tid query =
    let alias = printFromAlias tid
        StatementBuilder (t, ps) = printSelect p query
        a = addBracket t <> TLB.fromText " AS " <> alias
    in Clause . return . StatementBuilder $ (a, ps)

renderSelectorFields :: ExprPrinterType g -> FieldsSelector (SelWithAlias g) a -> Clause
renderSelectorFields _ (_ :$: Star' alias) = renderFieldWildcard alias
renderSelectorFields p (_ :$: Sel' a alias) = renderFieldClause (p Nothing Nothing a) alias
renderSelectorFields p (s :*: Star' alias) =
    renderSelectorFields p s <> renderFieldWildcard alias
renderSelectorFields p (s :*: Sel' a alias) =
    renderSelectorFields p s <> renderFieldClause (p Nothing Nothing a) alias

renderFieldWildcard :: RelationAlias a -> Clause
renderFieldWildcard (RelationAlias eid)      = renderFieldWildcardInternal eid
renderFieldWildcard (RelationAliasSub eid _) = renderFieldWildcardInternal eid

renderFieldWildcardInternal :: Int -> Clause
renderFieldWildcardInternal eid =
    let alias = printFromAlias eid
        a = alias <> TLB.fromText ".*"
    in Clause . return . StatementBuilder $ (a, mempty)

renderFieldClause :: StatementBuilder -> FieldAlias a -> Clause
renderFieldClause a (FieldAlias fid) =
    let alias = printFieldAlias fid
        StatementBuilder (e, ps) = a
        eas = e <> TLB.fromText " AS " <> alias
    in Clause . return . StatementBuilder $ (eas, ps)
