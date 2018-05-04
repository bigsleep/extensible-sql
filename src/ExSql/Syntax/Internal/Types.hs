module ExSql.Syntax.Internal.Types
    ( Ref(..)
    , FieldRef(..)
    ) where

data Ref a = Ref Int

data FieldRef a = FieldRef Int
