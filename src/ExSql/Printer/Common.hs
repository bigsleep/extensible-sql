{-# LANGUAGE OverloadedStrings #-}
module ExSql.Printer.Common
    ( addBracket
    , handleBracket
    , needBracket
    , needBracketL
    , needBracketR
    , printRelationAlias
    , printFieldAlias
    , printVals
    , printFun
    ) where

import qualified Data.List as List (intersperse)
import Data.Text (Text)
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Text.Lazy.Builder.Int as TLB
import ExSql.Printer.Types
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

printRelationAlias :: Int -> TLB.Builder
printRelationAlias tid =
    TLB.fromText prefix `mappend` TLB.decimal tid
    where
    prefix = "t_"

printFieldAlias :: Int -> TLB.Builder
printFieldAlias fid =
    TLB.fromText prefix `mappend` TLB.decimal fid
    where
    prefix = "f_"

printVals :: [StatementBuilder] -> StatementBuilder
printVals vals = StatementBuilder (t, ps)
    where
    xs = map (fst . unStatementBuilder) vals
    t = TLB.singleton '('
        `mappend` mconcat (List.intersperse (TLB.fromText ", ") xs)
        `mappend` TLB.singleton ')'
    ps = mconcat $ map (snd . unStatementBuilder) vals

printFun :: Text -> [StatementBuilder] -> StatementBuilder
printFun fname args = StatementBuilder (t, ps)
    where
    StatementBuilder (x, ps) = printVals args
    t = TLB.fromText fname `mappend` x
