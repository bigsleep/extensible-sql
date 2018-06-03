{-# LANGUAGE GADTs #-}
module ExSql.Syntax.Internal.Types
    ( Ref(..)
    , ValueList
    , PersistConvert
    ) where

import Control.Monad
import Control.Monad.Trans.State.Strict (StateT)
import Data.Text (Text)
import Database.Persist (Entity, PersistEntity(..), PersistField(..), PersistValue(..))

data Ref a where
    EntityRef :: (PersistEntity record) => Int -> Ref (Entity record)
    FieldRef :: (PersistField field) => Int -> Ref field

data ValueList a

type PersistConvert a = StateT [PersistValue] (Either Text) a
