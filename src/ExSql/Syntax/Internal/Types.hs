module ExSql.Syntax.Internal.Types
    ( Ref(..)
    , FieldRef(..)
    , PersistConvert
    ) where

import Data.Text (Text)
import qualified Database.Persist as Persist (PersistValue(..))

data Ref a = Ref Int

data FieldRef a = FieldRef Int

type PersistConvert a = [Persist.PersistValue] -> Either Text a
