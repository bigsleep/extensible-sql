{-# LANGUAGE OverloadedStrings, RankNTypes, GADTs #-}
module ExSql.Printer.SelectQuery
    ( renderSelect
    ) where

import Control.Monad.Trans.Writer.Strict (Writer)
import qualified Control.Monad.Trans.Writer.Strict as Writer (tell, runWriter)
import Data.DList (DList)
import qualified Data.DList as DList
import Data.Functor.Identity (Identity(..))
import Data.Proxy (Proxy(..), asProxyTypeOf)
import Data.Text (Text)
import qualified Data.Text as Text (unpack)
import qualified Database.Persist as Persist (DBName(..), Entity, EntityDef(..), PersistEntity(..), PersistValue(..))
import qualified Database.Persist.Sql.Util as Persist (parseEntityValues)

import ExSql.Syntax.Class
import ExSql.Syntax.Relativity
import ExSql.Syntax.SelectQuery

type PersistConvert a = [Persist.PersistValue] -> Either Text a

type SelectResult a = Writer SelectClauses (PersistConvert a)

data SelectClauses = SelectClauses
    { scFrom :: DList Text
    , scWhere :: DList Text
    , scPlaceholders :: DList Persist.PersistValue
    } deriving (Show)

instance Monoid SelectClauses where
    mempty = SelectClauses mempty mempty mempty
    mappend (SelectClauses from0 where0 ps0) (SelectClauses from1 where1 ps1) = SelectClauses (from0 `mappend` from1) (where0 `mappend` where1) (ps0 `mappend` ps1)

renderSelect :: (forall x. Maybe Relativity -> Maybe Relativity -> g x -> (Text, DList Persist.PersistValue)) -> SelectQuery g a -> (PersistConvert a, SelectClauses)
renderSelect p query = Writer.runWriter (renderSelectInternal p query)

renderSelectInternal :: (forall x. Maybe Relativity -> Maybe Relativity -> g x -> (Text, DList Persist.PersistValue)) -> SelectQuery g a -> SelectResult a
renderSelectInternal p (SelectFrom f) =
    let ref = undefined :: (Persist.PersistEntity record) => Ref record
        proxy = toProxy ref
        tableName = Persist.unDBName . Persist.entityDB . Persist.entityDef $ proxy
        clauses = SelectClauses (return tableName) mempty mempty
    in Writer.tell clauses >> renderSelectInternal p (f ref Initial)
renderSelectInternal p (Where cond query) = do
    let (t, ps) = p Nothing Nothing cond
        clauses =  SelectClauses mempty (return t) ps
    convert <- renderSelectInternal p query
    Writer.tell clauses
    return convert
renderSelectInternal _ i @ Initial =
    let def = Persist.entityDef (toProxy . asProxyTypeOf undefined $ i)
    in return (Persist.parseEntityValues def)

toProxy :: f a -> Proxy a
toProxy _ = Proxy
