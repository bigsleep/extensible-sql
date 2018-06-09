{-# LANGUAGE OverloadedStrings #-}
module ExSql.Printer.Common
    ( addBracket
    , handleBracket
    , needBracket
    , needBracketL
    , needBracketR
    , printFromAlias
    , printFieldAlias
    ) where

import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Text.Lazy.Builder.Int as TLB
import ExSql.Syntax.Relativity

addBracket :: TLB.Builder -> TLB.Builder
addBracket a = TLB.singleton '('
    `mappend` a
    `mappend` TLB.singleton ')'

handleBracket :: Maybe Relativity -> Relativity -> Maybe Relativity -> TLB.Builder -> TLB.Builder
handleBracket l c r s = if needBracket l c r
    then addBracket s
    else s

needBracket :: Maybe Relativity -> Relativity -> Maybe Relativity -> Bool
needBracket l c r = needBracketL c r || needBracketR l c

needBracketL :: Relativity -> Maybe Relativity -> Bool
needBracketL (Relativity p a) (Just (Relativity rp _))
    | p > rp = True
    | a == NonAssociative = True
    | p == rp && a == LeftToRight = True
    | otherwise = False
needBracketL _ _ = False

needBracketR :: Maybe Relativity -> Relativity -> Bool
needBracketR (Just (Relativity lp _)) (Relativity p a)
    | p > lp = True
    | a == NonAssociative = True
    | p == lp && a == RightToLeft = True
    | otherwise = False
needBracketR _ _ = False

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
