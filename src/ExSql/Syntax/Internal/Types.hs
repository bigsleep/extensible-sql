{-# LANGUAGE GADTs #-}
module ExSql.Syntax.Internal.Types
    ( ValueList
    , PersistConvert
    , Ref(..)
    , FieldAlias(..)
    , FieldRef(..)
    ) where

import Control.Monad
import Control.Monad.Trans.State.Strict (StateT)
import Data.Text (Text)
import Database.Persist (Entity, PersistEntity(..), PersistField(..), PersistValue(..))

newtype Ref a = Ref Int
    deriving (Show, Eq)

newtype FieldRef a = FieldRef Int
    deriving (Show, Eq)

newtype FieldAlias a = FieldAlias Int
    deriving (Show, Eq)

data ValueList a

type PersistConvert a = StateT [PersistValue] (Either Text) a
