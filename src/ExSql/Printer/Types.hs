{-# LANGUAGE KindSignatures, RankNTypes #-}
module ExSql.Printer.Types
    ( ExprPrinterType
    , PrinterType
    , Printer(..)
    ) where

import Data.DList (DList)
import Data.Text (Text)
import Database.Persist (PersistValue(..))
import ExSql.Syntax.Relativity

type ExprPrinterType g =
    forall a. Maybe Relativity -> Maybe Relativity -> g a -> (Text, DList PersistValue)

type PrinterType (g :: * -> *) (v :: (* -> *) -> * -> *) a =
    Maybe Relativity -> Maybe Relativity -> v g a -> (Text, DList PersistValue)

newtype Printer (g :: * -> *) (v :: (* -> *) -> * -> *) = Printer
    { runPrinter :: forall a. PrinterType g v a }
