module ExSql.Syntax.Internal.Types
    ( Ref(..)
    , FieldRef(..)
    , ValueList
    , PersistConvert
    ) where

import Data.Text (Text)
import qualified Database.Persist as Persist (PersistValue(..))

data Ref a = Ref Int

data FieldRef a = FieldRef Int

data ValueList a

type PersistConvert a = [Persist.PersistValue] -> Either Text a
