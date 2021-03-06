{-# LANGUAGE GADTs #-}
module ExSql.Syntax.Internal.Types
    ( AggregatedE(..)
    , ConvertResult(..)
    , FRef(..)
    , FieldAlias(..)
    , JoinType(..)
    , PersistConvert
    , RRef(..)
    , Ref(..)
    , SelectStage(..)
    , ValueList
    , rrefId
    ) where

import Control.Monad.Trans.State.Strict (StateT)
import Data.Text (Text)
import qualified Database.Persist as Persist (PersistField, PersistValue)

data SelectStage =
    Neutral |
    FieldsSpecified |
    Aggregated |
    AggFieldsSpecified
    deriving (Show, Eq)

data AggregatedE = AggregatedE

newtype RRef a = RRef Int

rrefId :: RRef a -> Int
rrefId (RRef i) = i

newtype FieldAlias a = FieldAlias Int
    deriving (Show, Eq)

data FRef a = FRef Int | QRef Int Int
    deriving (Show, Eq)

data Ref a where
    RelationRef :: RRef a -> Ref a
    FieldRef :: (Persist.PersistField a) => FRef a -> Ref a

data ValueList a

data ConvertResult =
    HitNullValue |
    ConvertError Text
    deriving (Show, Eq)

type PersistConvert a = StateT [Persist.PersistValue] (Either ConvertResult) a

data JoinType =
    InnerJoin |
    CrossJoin |
    LeftJoin |
    RightJoin |
    FullJoin
    deriving (Show, Eq)
