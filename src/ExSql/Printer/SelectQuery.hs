{-# LANGUAGE OverloadedStrings, RankNTypes, GADTs, GeneralizedNewtypeDeriving #-}
module ExSql.Printer.SelectQuery
    ( SelectResult
    , Clause(..)
    , SelectClauses(..)
    , renderSelect
    ) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT)
import qualified Control.Monad.Trans.Reader as Reader (ask, runReaderT)
import Control.Monad.Trans.State.Strict (State, StateT)
import qualified Control.Monad.Trans.State.Strict as State (get, modify', put, evalStateT)
import Control.Monad.Trans.Writer.Strict (Writer)
import qualified Control.Monad.Trans.Writer.Strict as Writer (tell, runWriter)
import Data.DList (DList)
import qualified Data.DList as DList
import Data.Functor.Identity (Identity(..))
import Data.Maybe (maybe)
import Data.Proxy (Proxy(..), asProxyTypeOf)
import Data.Text (Text)
import qualified Data.Text as Text (unpack)
import qualified Database.Persist as Persist (DBName(..), Entity, EntityDef(..), PersistEntity(..), PersistField(..), PersistValue(..))
import qualified Database.Persist.Sql.Util as Persist (parseEntityValues)
import Safe (atMay)

import ExSql.Syntax.Class
import ExSql.Syntax.Relativity
import ExSql.Syntax.SelectQuery
import ExSql.Syntax.Internal.Types

type SelectResult a = StateT (Int, Int) (Writer SelectClauses) (PersistConvert a)

newtype Clause = Clause (DList (Text, DList Persist.PersistValue))
    deriving (Show, Monoid, Eq)

data SelectClauses = SelectClauses
    { scField :: !Clause
    , scFrom :: !Clause
    , scWhere :: !Clause
    } deriving (Show, Eq)

instance Monoid SelectClauses where
    mempty = SelectClauses mempty mempty mempty
    mappend (SelectClauses field0 from0 where0) (SelectClauses field1 from1 where1) = SelectClauses (field0 `mappend` field1) (from0 `mappend` from1) (where0 `mappend` where1)

renderSelect :: (forall x. Maybe Relativity -> Maybe Relativity -> g x -> (Text, DList Persist.PersistValue)) -> SelectQuery g a -> (PersistConvert a, SelectClauses)
renderSelect p query = Writer.runWriter . flip State.evalStateT (0, 0) $ (renderSelectInternal p query)

renderSelectInternal :: (forall x. Maybe Relativity -> Maybe Relativity -> g x -> (Text, DList Persist.PersistValue)) -> SelectQuery g a -> SelectResult a
renderSelectInternal p (SelectFrom f) = do
    (i, j) <- State.get
    State.put (i + 1, j)
    let ref = Ref i :: (Persist.PersistEntity record) => Ref record
        proxy = toProxy ref
        tableName = Persist.unDBName . Persist.entityDB . Persist.entityDef $ proxy
        clauses = mempty { scFrom = (Clause . return $ (tableName, mempty)) }
    lift . Writer.tell $ clauses
    renderSelectInternal p (f ref Initial)
renderSelectInternal p (ResultAs selector f query) = do
    (i, j) <- State.get
    let (convert', next) = mkPersistConvert j selector
        convert = Reader.runReaderT convert'
        clauses = mempty { scField = (renderSelectorFields p selector) }
        (fieldRef, _) = mkSelectorFieldRef j selector
    State.put (i, next)
    lift . Writer.tell $ clauses
    renderSelectInternal p query
    renderSelectInternal p (f fieldRef (Transform convert))
renderSelectInternal p (Where cond query) = do
    let a = p Nothing Nothing cond
        clauses =  SelectClauses mempty mempty (Clause . return $ a)
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

renderSelectorFields :: (forall x. Maybe Relativity -> Maybe Relativity -> g x -> (Text, DList Persist.PersistValue)) -> Selector g a -> Clause
renderSelectorFields p (_ :$ a) = Clause . return . p Nothing Nothing $ a
renderSelectorFields p (s :* a) = renderSelectorFields p s `mappend` (Clause . return . p Nothing Nothing $ a)

mkSelectorFieldRef :: Int -> Selector g a -> (Selector FieldRef a, Int)
mkSelectorFieldRef i (f :$ a) = (f :$ toFieldRef i a, i + 1)
mkSelectorFieldRef i (s :* a) =
    let (r, next) = mkSelectorFieldRef i s
    in (r :* toFieldRef next a, next + 1)

toFieldRef :: Int -> g a -> FieldRef a
toFieldRef index _ = FieldRef index
