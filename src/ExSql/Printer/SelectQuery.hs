{-# LANGUAGE OverloadedStrings, RankNTypes, GADTs, GeneralizedNewtypeDeriving #-}
module ExSql.Printer.SelectQuery
    ( renderSelect
    ) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (State, StateT)
import qualified Control.Monad.Trans.State.Strict as State (get, gets, modify', put, evalStateT)
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

type PersistConvert a = [Persist.PersistValue] -> Either Text a

type SelectResult a = StateT (Int, Int) (Writer SelectClauses) (PersistConvert a)

newtype Clause = Clause (DList (Text, DList Persist.PersistValue))
    deriving (Show, Monoid)

data SelectClauses = SelectClauses
    { scField :: !Clause
    , scFrom :: !Clause
    , scWhere :: !Clause
    } deriving (Show)

instance Monoid SelectClauses where
    mempty = SelectClauses mempty mempty mempty
    mappend (SelectClauses field0 from0 where0) (SelectClauses field1 from1 where1) = SelectClauses (field0 `mappend` field1) (from0 `mappend` from1) (where0 `mappend` where1)

renderSelect :: (forall x. Maybe Relativity -> Maybe Relativity -> g x -> (Text, DList Persist.PersistValue)) -> SelectQuery g a -> (PersistConvert a, SelectClauses)
renderSelect p query = Writer.runWriter . flip State.evalStateT (0, 0) $ (renderSelectInternal p query)

renderSelectInternal :: (forall x. Maybe Relativity -> Maybe Relativity -> g x -> (Text, DList Persist.PersistValue)) -> SelectQuery g a -> SelectResult a
renderSelectInternal p (SelectFrom f) = do
    index <- State.gets fst
    State.modify' $ \(i, j) -> (i + 1, j)
    let ref = Ref index :: (Persist.PersistEntity record) => Ref record
        proxy = toProxy ref
        tableName = Persist.unDBName . Persist.entityDB . Persist.entityDef $ proxy
        clauses = SelectClauses mempty (Clause . return $ (tableName, mempty)) mempty
    lift . Writer.tell $ clauses
    renderSelectInternal p (f ref Initial)
renderSelectInternal p (Where cond query) = do
    let a = p Nothing Nothing cond
        clauses =  SelectClauses mempty mempty (Clause . return $ a)
    convert <- renderSelectInternal p query
    lift . Writer.tell $ clauses
    return convert
renderSelectInternal _ i @ Initial =
    let def = Persist.entityDef (toProxy . asProxyTypeOf undefined $ i)
    in return (Persist.parseEntityValues def)

toProxy :: f a -> Proxy a
toProxy _ = Proxy

extractSelector :: Selector g a -> [Persist.PersistValue] -> StateT Int (Either Text) a
extractSelector (f :$ a) vals = do
    index <- State.get
    val <- maybe (lift . Left $ "index out of range") return (vals `atMay` index)
    r <- lift . Persist.fromPersistValue $ val
    State.put (index + 1)
    return (f r)
extractSelector (s :* a) vals = do
    f <- extractSelector s vals
    index <- State.get
    val <- maybe (lift . Left $ "index out of range") return (vals `atMay` index)
    r <- lift . Persist.fromPersistValue $ val
    State.put (index + 1)
    return (f r)

renderSelectorFields :: (forall x. Maybe Relativity -> Maybe Relativity -> g x -> (Text, DList Persist.PersistValue)) -> Selector g a -> Clause
renderSelectorFields p (_ :$ a) = Clause . return . p Nothing Nothing $ a
renderSelectorFields p (s :* a) = renderSelectorFields p s `mappend` (Clause . return . p Nothing Nothing $ a)

mkSelectorFieldRef :: Selector g a -> State Int (Selector FieldRef a)
mkSelectorFieldRef (f :$ a) = do
    index <- State.get
    State.put (index + 1)
    return (f :$ toFieldRef index a)
mkSelectorFieldRef (s :* a) = do
    r <- mkSelectorFieldRef s
    index <- State.get
    State.put (index + 1)
    return (r :* toFieldRef index a)

toFieldRef :: Int -> g a -> FieldRef a
toFieldRef index _ = FieldRef index
