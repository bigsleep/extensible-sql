{-# LANGUAGE OverloadedStrings, RankNTypes, GADTs, GeneralizedNewtypeDeriving #-}
module ExSql.Printer.SelectQuery
    ( SelectResult
    , Clause(..)
    , SelectClauses(..)
    , renderSelect
    , printFieldRef
    , printSelect
    ) where

import Control.Monad (MonadPlus(..))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT)
import qualified Control.Monad.Trans.Reader as Reader (ask, runReaderT)
import Control.Monad.Trans.State.Strict (State, StateT)
import qualified Control.Monad.Trans.State.Strict as State (get, modify', put, evalStateT)
import Control.Monad.Trans.Writer.Strict (Writer)
import qualified Control.Monad.Trans.Writer.Strict as Writer (tell, runWriter)
import Data.Int (Int64)
import Data.List (intersperse)
import Data.DList (DList)
import qualified Data.DList as DList
import Data.Functor.Identity (Identity(..))
import Data.Maybe (maybe)
import Data.Proxy (Proxy(..), asProxyTypeOf)
import Data.Semigroup (Semigroup(..))
import Data.Text (Text)
import qualified Data.Text as Text (unpack)
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Text.Lazy.Builder.Int as TLB
import qualified Database.Persist as Persist (DBName(..), Entity, EntityDef(..), PersistEntity(..), PersistField(..), PersistValue(..))
import qualified Database.Persist.Sql.Util as Persist (parseEntityValues)
import Safe (atMay)

import ExSql.Printer.Types
import ExSql.Syntax.Class
import ExSql.Syntax.Relativity
import ExSql.Syntax.SelectQuery
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

printSelect :: ExprPrinterType g -> SelectQuery g a -> StatementBuilder
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
printWhereClause (Clause xs) = StatementBuilder (t, mconcat ps)
    where
    (ts, ps) = unzip . map unStatementBuilder . DList.toList $ xs
    t = TLB.fromText " WHERE "
        <> (mconcat . intersperse (TLB.fromText " AND ") $ ts)

printOrderByClause :: OrderByClause -> StatementBuilder
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

renderSelect :: ExprPrinterType g -> SelectQuery g a -> (PersistConvert a, SelectClauses)
renderSelect p query = Writer.runWriter . flip State.evalStateT (0, 0) $ (renderSelectInternal p query)

renderSelectInternal :: ExprPrinterType g -> SelectQuery g a -> SelectResult a
renderSelectInternal p (SelectFrom f) = do
    (i, j) <- State.get
    State.put (i + 1, j)
    let ref = Ref i :: (Persist.PersistEntity record) => Ref record
        proxy = toProxy ref
        tableName = Persist.unDBName . Persist.entityDB . Persist.entityDef $ proxy
        clauses = mempty { scFrom = (Clause . return . StatementBuilder $ (TLB.fromText tableName, mempty)) }
    lift . Writer.tell $ clauses
    renderSelectInternal p (f ref Initial)
renderSelectInternal p (ResultAs selector f query) = do
    (i, j) <- State.get
    let (convert', next) = mkPersistConvert j selector
        convert = Reader.runReaderT convert'
        (fieldRef, _) = mkSelectorFieldRef j selector
        (fieldClause, _) = renderSelectorFields j p selector
        clauses = mempty { scField = fieldClause }
    State.put (i, next)
    lift . Writer.tell $ clauses
    renderSelectInternal p query
    renderSelectInternal p (f fieldRef (Transform convert))
renderSelectInternal p (Where cond query) = do
    let relativity = Just (Relativity (Precedence 13) RightToLeft)
        a = p relativity relativity cond
        clauses = mempty { scWhere = Clause . return $ a }
    convert <- renderSelectInternal p query
    lift . Writer.tell $ clauses
    return convert
renderSelectInternal p (OrderBy a order query) = do
    let StatementBuilder (t, ps) = p Nothing Nothing a
        clauses = mempty { scOrderBy = OrderByClause . return $ (StatementBuilder (t, ps), order) }
    convert <- renderSelectInternal p query
    lift . Writer.tell $ clauses
    return convert
renderSelectInternal p (Limit limit query) = do
    let clauses = mempty { scLimit = LimitClause Nothing (Just limit) }
    convert <- renderSelectInternal p query
    lift . Writer.tell $ clauses
    return convert
renderSelectInternal p (Offset offset query) = do
    let clauses = mempty { scLimit = LimitClause (Just offset) Nothing }
    convert <- renderSelectInternal p query
    lift . Writer.tell $ clauses
    return convert
renderSelectInternal _ i @ Initial =
    let def = Persist.entityDef (toProxy . asProxyTypeOf undefined $ i)
    in return (Persist.parseEntityValues def)
renderSelectInternal _ (Transform convert) = return convert

toProxy :: f a -> Proxy a
toProxy _ = Proxy

mkPersistConvert :: Int -> Selector g a -> (ReaderT [Persist.PersistValue] (Either Text) a, Int)
mkPersistConvert i (f :$ _) = (mkPersistConvertInternal i f, i + 1)
mkPersistConvert i (s :* _) =
    let (m, j) = mkPersistConvert i s
        n = m >>= mkPersistConvertInternal j
    in (n, j + 1)
mkPersistConvertInternal k f = do
    vals <- Reader.ask
    r <- maybe (lift . Left $ "index out of range") (lift . Persist.fromPersistValue) (vals `atMay` k)
    return (f r)

renderSelectorFields :: Int -> ExprPrinterType g -> Selector g a -> (Clause, Int)
renderSelectorFields i p (_ :$ a) = (renderFieldClause p (FieldRef i) a, i + 1)
renderSelectorFields i p (s :* a) =
    let (r, next) = renderSelectorFields i p s
        clause = r <> renderFieldClause p (FieldRef next) a
    in (clause, next + 1)

mkSelectorFieldRef :: Int -> Selector g a -> (Selector FieldRef a, Int)
mkSelectorFieldRef i (f :$ a) = (f :$ toFieldRef i a, i + 1)
mkSelectorFieldRef i (s :* a) =
    let (r, next) = mkSelectorFieldRef i s
    in (r :* toFieldRef next a, next + 1)

toFieldRef :: Int -> g a -> FieldRef a
toFieldRef index _ = FieldRef index

printFieldRef :: FieldRef a -> TLB.Builder
printFieldRef (FieldRef fid) =
    let prefix = "f_"
    in TLB.fromText prefix
        <> TLB.decimal fid

renderFieldClause :: ExprPrinterType g -> FieldRef a -> g a -> Clause
renderFieldClause p fr a =
    let refName = printFieldRef fr
        StatementBuilder (e, ps) = p Nothing Nothing $ a
        eas = e <> TLB.fromText " AS " <> refName
    in Clause . return . StatementBuilder $ (eas, ps)
