{-# LANGUAGE GADTs #-}
module ExSql.Syntax.Internal.Types
    ( FieldAlias(..)
    , FieldRef(..)
    , PersistConvert
    , RelationAlias(..)
    , RelationRef(..)
    , ValueList
    ) where

import Control.Monad
import Control.Monad.Trans.State.Strict (StateT)
import Data.Text (Text)
import Database.Persist (Entity, PersistEntity(..), PersistField(..), PersistValue(..))

newtype RelationRef a = RelationRef Int
    deriving (Show, Eq)

newtype RelationAlias a = RelationAlias Int
    deriving (Show, Eq)

data FieldRef a = FieldRef Int | QualifiedFieldRef Int Int
    deriving (Show, Eq)

newtype FieldAlias a = FieldAlias Int
    deriving (Show, Eq)

data ValueList a

type PersistConvert a = StateT [PersistValue] (Either Text) a
