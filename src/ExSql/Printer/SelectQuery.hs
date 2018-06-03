{-# LANGUAGE OverloadedStrings, RankNTypes, GADTs, GeneralizedNewtypeDeriving #-}
module ExSql.Printer.SelectQuery
    ( SelectResult
    , Clause(..)
    , SelectClauses(..)
    , renderSelect
    , printFromAlias
    , printFieldAlias
    , printSelect
    , printSelectClauses
    ) where

import Control.Monad (MonadPlus(..))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT)
import qualified Control.Monad.Trans.Reader as Reader (ask, runReaderT)
import Control.Monad.Trans.State.Strict (State, StateT)
import qualified Control.Monad.Trans.State.Strict as State (get, modify', put, evalStateT)
import Control.Monad.Trans.Writer.Strict (Writer)
import qualified Control.Monad.Trans.Writer.Strict as Writer (tell, runWriter)
import Data.Foldable (fold)
import Data.Int (Int64)
import Data.List (intersperse, uncons)
import Data.DList (DList)
import qualified Data.DList as DList
import Data.Functor.Identity (Identity(..))
import Data.Functor.Product (Product(..))
import Data.Maybe (maybe)
import Data.Proxy (Proxy(..))
import Data.Semigroup (Semigroup(..))
import Data.Text (Text)
import qualified Data.Text as Text (unpack)
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Text.Lazy.Builder.Int as TLB
import qualified Database.Persist as Persist (DBName(..), Entity(..), EntityDef(..), PersistEntity(..), PersistField(..), PersistValue(..))
import qualified Database.Persist.Sql as Persist (tableDBName)
import qualified Database.Persist.Sql.Util as Persist (entityColumnCount, parseEntityValues)
import Safe.Exact (splitAtExactMay)

import ExSql.Printer.Types
import ExSql.Syntax.Class
import ExSql.Syntax.Relativity
import ExSql.Syntax.SelectQuery (SelectQuery(..), Selector(..), OrderType(..))
import qualified ExSql.Syntax.SelectQuery as Syntax
import ExSql.Syntax.Internal.Types

type SelectResult a = StateT (Int, Int) (Writer SelectClauses) (PersistConvert a)

newtype Clause = Clause (DList StatementBuilder)
    deriving (Show, Semigroup, Monoid, Eq)

newtype OrderByClause = OrderByClause (DList (StatementBuilder, OrderType))
    deriving (Show, Semigroup, Monoid, Eq)

data LimitClause = LimitClause
    { lcOffset :: Maybe Int64
    , lcLimit :: Maybe Int64
    } deriving (Show, Eq)

data SelectClauses = SelectClauses
    { scField :: !Clause
    , scFrom :: !Clause
    , scWhere :: !Clause
    , scOrderBy :: !OrderByClause
    , scLimit :: !LimitClause
    } deriving (Show, Eq)

instance Semigroup SelectClauses where
    (<>) (SelectClauses field0 from0 where0 orderBy0 (LimitClause offset0 limit0)) (SelectClauses field1 from1 where1 orderBy1 (LimitClause offset1 limit1)) =
        SelectClauses (field0 <> field1) (from0 <> from1) (where0 <> where1) (orderBy0 <> orderBy1) (LimitClause (offset0 `mplus` offset1) (limit0 `mplus` limit1))

instance Monoid SelectClauses where
    mempty = SelectClauses mempty mempty mempty mempty (LimitClause Nothing Nothing)
    mappend = (<>)

printSelect :: ExprPrinterType g -> Syntax.SelectQuery g a -> StatementBuilder
printSelect p query =
    let (_, sc) = renderSelect p query
    in printSelectClauses sc

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
printOrderByClause (OrderByClause xs) = StatementBuilder (t, mconcat ps)
    where
    ys = DList.toList xs
    ps = map (snd . unStatementBuilder . fst) $ ys
    ts = map (\(StatementBuilder (t, _), order) -> t <> TLB.fromText (showOrder order)) ys
    t = TLB.fromText " ORDER BY " <> (mconcat . intersperse (TLB.fromText ", ") $ ts)
    showOrder Asc = "ASC"
    showOrder Desc = "DESC"

printLimitClause (LimitClause Nothing Nothing) = mempty
printLimitClause (LimitClause (Just offset) Nothing) = StatementBuilder (TLB.fromText " OFFSET " <> TLB.decimal offset, mempty)
printLimitClause (LimitClause Nothing (Just limit)) = StatementBuilder (TLB.fromText " LIMIT " <> TLB.decimal limit, mempty)
printLimitClause (LimitClause (Just offset) (Just limit)) = StatementBuilder (TLB.fromText " OFFSET " <> TLB.decimal offset <> TLB.fromText " LIMIT " <> TLB.decimal limit, mempty)

renderSelect :: ExprPrinterType g -> Syntax.SelectQuery g a -> (PersistConvert a, SelectClauses)
renderSelect p query = (convert, clauses)
    where
    (sref, Syntax.SelectClauses sclauses) = Writer.runWriter . flip State.evalStateT (0, 0) . unSelectQuery $ query
    convert = mkPersistConvert sref
    clauses = foldMap (renderSelectClause p) sclauses

renderSelectClause :: ExprPrinterType g -> Syntax.SelectClause g -> SelectClauses
renderSelectClause p (Syntax.Fields fs) = mempty { scField = renderSelectorFields p fs }
renderSelectClause p (Syntax.From a) = mempty { scFrom = renderFrom p a }
renderSelectClause p (Syntax.Where w) = mempty { scWhere = Clause . return . p Nothing Nothing $ w }
renderSelectClause p (Syntax.OrderBy a t) = mempty { scOrderBy = OrderByClause . return $ (p Nothing Nothing a, t) }
renderSelectClause p (Syntax.Limit limit) = mempty { scLimit = LimitClause Nothing (Just limit) }
renderSelectClause p (Syntax.Offset offset) = mempty { scLimit = LimitClause (Just offset) Nothing }
renderSelectClause _ Syntax.Initial = mempty

toProxy :: f a -> Proxy a
toProxy _ = Proxy

mkPersistConvert :: Selector g a -> PersistConvert a
mkPersistConvert (Sel a) = do
    let def = Persist.entityDef . fmap Persist.entityVal . toProxy $ a
        colNum = Persist.entityColumnCount def
    xs <- State.get
    (vals, rest) <- maybe (lift . Left $ "not enough input values") return (splitAtExactMay colNum xs)
    lift $ Persist.parseEntityValues def vals
mkPersistConvert (f :$ _) = mkPersistConvertInternal f
mkPersistConvert (s :* _) = mkPersistConvert s >>= mkPersistConvertInternal
mkPersistConvertInternal f = do
    xs <- State.get
    (val, rest) <- maybe (lift . Left $ "not enough input values") return (uncons xs)
    State.put rest
    r <- lift . Persist.fromPersistValue $ val
    return (f r)

printFromAlias :: Int -> TLB.Builder
printFromAlias tid =
    let prefix = "t_"
    in TLB.fromText prefix
        <> TLB.decimal tid

renderFrom :: (Persist.PersistEntity record) => ExprPrinterType g -> Ref (Persist.Entity record) -> Clause
renderFrom p ref @ (EntityRef eid) =
    let tableName = Persist.unDBName . Persist.entityDB . Persist.entityDef . fmap Persist.entityVal . toProxy $ ref
        alias = printFromAlias eid
        a = TLB.fromText tableName <> TLB.fromText " AS " <> alias
    in Clause . return . StatementBuilder $ (a, mempty)

renderSelectorFields :: ExprPrinterType g -> Selector (Product g Ref) a -> Clause
renderSelectorFields p (Sel ref) = renderFieldClause mempty ref
renderSelectorFields p (_ :$ Pair a ref) = renderFieldClause (p Nothing Nothing a) ref
renderSelectorFields p (s :* Pair a ref) =
    renderSelectorFields p s <> renderFieldClause (p Nothing Nothing a) ref

printFieldAlias :: Int -> TLB.Builder
printFieldAlias fid =
    let prefix = "f_"
    in TLB.fromText prefix
        <> TLB.decimal fid

renderFieldClause :: StatementBuilder -> Ref a -> Clause
renderFieldClause a (EntityRef eid) =
    let alias = printFromAlias eid
        a = alias <> TLB.fromText ".*"
    in Clause . return . StatementBuilder $ (a, mempty)
renderFieldClause a (FieldRef fid) =
    let alias = printFieldAlias fid
        StatementBuilder (e, ps) = a
        eas = e <> TLB.fromText " AS " <> alias
    in Clause . return . StatementBuilder $ (eas, ps)
