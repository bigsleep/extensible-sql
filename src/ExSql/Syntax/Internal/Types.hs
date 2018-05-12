module ExSql.Syntax.Internal.Types
    ( Ref(..)
    , FieldRef(..)
    , ValueList
    , PersistConvert
    ) where

import Data.Text (Text)
import Database.Persist (PersistValue(..), PersistField(..))

data Ref a = Ref Int

data FieldRef a = FieldRef Int

data ValueList a

type PersistConvert a = [PersistValue] -> Either Text a
