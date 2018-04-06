module ExSql.Syntax.Relativity
    ( Precedence(..)
    , Associativity(..)
    , Relativity(..)
    ) where

newtype Precedence = Precedence { unPrecedence :: Int } deriving (Show, Eq, Ord)

data Associativity
    = LeftToRight
    | RightToLeft
    | NonAssociative
    deriving (Show, Eq)

data Relativity = Relativity
    { relativityPrecedence :: Precedence
    , relativityAssociativity :: Associativity
    } deriving (Show, Eq)
