{-# LANGUAGE KindSignatures, RankNTypes #-}
module ExSql.Printer.Types
    ( ExprPrinterType
    , PrinterType
    , Printer(..)
    ) where

import Data.DList (DList)
import qualified Data.Text.Lazy.Builder as TLB (Builder)
import Database.Persist (PersistValue(..))
import ExSql.Syntax.Relativity

type ExprPrinterType g =
    forall a. Maybe Relativity -> Maybe Relativity -> g a -> (TLB.Builder, DList PersistValue)

type PrinterType (g :: * -> *) (v :: (* -> *) -> * -> *) a =
    Maybe Relativity -> Maybe Relativity -> v g a -> (TLB.Builder, DList PersistValue)

newtype Printer (g :: * -> *) (v :: (* -> *) -> * -> *) = Printer
    { runPrinter :: forall a. PrinterType g v a }
