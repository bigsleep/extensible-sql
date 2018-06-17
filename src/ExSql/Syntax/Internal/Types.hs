{-# LANGUAGE GADTs #-}
module ExSql.Syntax.Internal.Types
    ( FRef(..)
    , FieldAlias(..)
    , PersistConvert
    , Ref(..)
    , RRef(..)
    , RelationAlias(..)
    , Sel(..)
    , SelWithAlias(..)
    , ValueList
    ) where

import Control.Monad
import Control.Monad.Trans.State.Strict (StateT)
import Data.Text (Text)
import Database.Persist (Entity, PersistEntity(..), PersistField(..),
                         PersistValue(..))
import ExSql.Syntax.Class

newtype RelationAlias a = RelationAlias Int
    deriving (Show, Eq)

newtype RRef a = RRef Int
    deriving (Show, Eq)

newtype FieldAlias a = FieldAlias Int
    deriving (Show, Eq)

data FRef a = FRef Int | QRef Int Int
    deriving (Show, Eq)

data Ref a where
    RelationRef :: (PersistEntity a) => RRef (Entity a) -> Ref (Entity a)
    FieldRef :: (PersistField a) => FRef a -> Ref a

data Sel g a where
    Star :: (PersistEntity a) => RelationAlias (Entity a) -> Sel g (Entity a)
    Sel :: (PersistField a) => g a -> Sel g a

data SelWithAlias g a where
    Star' :: (PersistEntity a) => RelationAlias (Entity a) -> SelWithAlias g (Entity a)
    Sel' :: (PersistField a) => g a -> FieldAlias a -> SelWithAlias g a

data ValueList a

type PersistConvert a = StateT [PersistValue] (Either Text) a

instance Hoist Sel where
    hoist _ (Star a) = Star a
    hoist f (Sel a)  = Sel (f a)

instance Hoist SelWithAlias where
    hoist _ (Star' a)  = Star' a
    hoist f (Sel' a b) = Sel' (f a) b
