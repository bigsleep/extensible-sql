{-# LANGUAGE TypeOperators #-}
module ExSql.Printer.Default
    ( print
    ) where

import Data.Extensible ((:*), Match, match)
import ExSql.Syntax.Class (Expr(..), Node)

pretty :: Match (Node (Expr xs) a) r :* xs -> Expr xs a -> r
pretty printers (Expr expr) = match printers expr
