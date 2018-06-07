{-# LANGUAGE OverloadedStrings #-}
module ExSql.Printer.Common
    ( printFromAlias
    , printFieldAlias
    ) where

import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Text.Lazy.Builder.Int as TLB

printFromAlias :: Int -> TLB.Builder
printFromAlias tid =
    TLB.fromText prefix `mappend` TLB.decimal tid
    where
    prefix = "t_"

printFieldAlias :: Int -> TLB.Builder
printFieldAlias fid =
    TLB.fromText prefix `mappend` TLB.decimal fid
    where
    prefix = "f_"
