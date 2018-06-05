{-# LANGUAGE
    KindSignatures,
    RankNTypes
#-}
module ExSql.Printer.Types
    ( ExprPrinterType
    , PrinterType
    , Printer(..)
    , StatementBuilder(..)
    ) where

import Data.DList (DList)
import Data.Semigroup (Semigroup(..))
import qualified Data.Text.Lazy.Builder as TLB (Builder)
import Database.Persist (PersistValue(..))
import ExSql.Syntax.Relativity

newtype StatementBuilder = StatementBuilder
    { unStatementBuilder :: (TLB.Builder, DList PersistValue)
    } deriving (Show, Eq)

instance Semigroup StatementBuilder where
    (<>) (StatementBuilder (t0, ps0)) (StatementBuilder (t1, ps1)) = StatementBuilder (t0 <> t1, ps0 <> ps1)

instance Monoid StatementBuilder where
    mempty = StatementBuilder (mempty, mempty)
    mappend = (<>)

type ExprPrinterType g =
    forall a. Maybe Relativity -> Maybe Relativity -> g a -> StatementBuilder

type PrinterType (g :: * -> *) (v :: (* -> *) -> * -> *) a =
    Maybe Relativity -> Maybe Relativity -> v g a -> StatementBuilder

newtype Printer (g :: * -> *) (v :: (* -> *) -> * -> *) = Printer
    { runPrinter :: forall a. PrinterType g v a }
