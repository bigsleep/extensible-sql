{-# LANGUAGE DataKinds, FlexibleContexts, GADTs, GeneralizedNewtypeDeriving, KindSignatures, MultiParamTypeClasses, RankNTypes, TypeFamilies, TypeOperators, QuasiQuotes, TemplateHaskell #-}
module ExSql.Syntax.ColumnSpec
    ( spec
    ) where

import Control.Monad.Reader.Class (MonadReader(..))
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Data.Extensible (Member, Match(..), (:|)(..), (:*), (<:), nil, hindex)
import Data.Functor.Identity (Identity(..))
import Data.Proxy (Proxy(..))
import qualified Data.Text as Text (unpack)
import qualified Database.Persist as Persist (DBName(..))
import qualified Database.Persist.Sql as Persist (fieldDBName)
import qualified Database.Persist.TH as Persist (mkPersist, persistLowerCase, share, sqlSettings)
import ExSql.Syntax.Column
import ExSql.Syntax.Comparison
import ExSql.Syntax.Class
import ExSql.Syntax.Literal
import Test.Hspec

Persist.share [Persist.mkPersist Persist.sqlSettings] [Persist.persistLowerCase|
Person
    name String
    age Int
    deriving Show
|]

newtype Printer (g :: * -> *) (v :: (* -> *) -> * -> *) = Printer { runPrinter :: forall a. v g a -> String }
type Nodes = '[Column, Comparison, Literal]
type E = Expr Nodes Identity
type Printers xs a = Printer (Expr xs Identity) :* xs

ppComparison :: (forall b. Expr xs Identity b -> String) -> Comparison (Expr xs Identity) a -> String
ppComparison p (Equality a0 a1) = "(" `mappend` p a0 `mappend` "==" `mappend` p a1 `mappend` ")"
ppComparison p (GreaterThan a0 a1) = "(" `mappend` p a0 `mappend` ">" `mappend` p a1 `mappend` ")"
ppComparison p (LessThan a0 a1) = "(" `mappend` p a0 `mappend` "<" `mappend` p a1 `mappend` ")"
ppComparison p (GreaterThanOrEqual a0 a1) = "(" `mappend` p a0 `mappend` ">=" `mappend` p a1 `mappend` ")"
ppComparison p (LessThanOrEqual a0 a1) = "(" `mappend` p a0 `mappend` "<=" `mappend` p a1 `mappend` ")"

ppLiteral :: Literal (Expr xs Identity) a -> String
ppLiteral (LitInt a) = show a
ppLiteral (LitBool a) = show a

ppColumn :: Column (Expr xs Identity) a -> String
ppColumn (Column field) = "`" `mappend` (Text.unpack . Persist.unDBName . Persist.fieldDBName $ field) `mappend` "`"

printers :: (forall b. Expr Nodes Identity b -> String) -> Printers Nodes a
printers p = Printer ppColumn
    <: Printer (ppComparison p)
    <: Printer ppLiteral
    <: nil

runPrinters :: Printer (Expr xs Identity) :* xs -> Expr xs Identity a -> String
runPrinters printers (Expr (EmbedAt p (Node (Identity a)))) = runPrinter (hindex printers p) a

pp :: Expr Nodes Identity a -> String
pp = runPrinters (printers pp)

e1 :: (Ast g, MonadReader (proxy Person) m, Member (NodeTypes g) Column, Member (NodeTypes g) Comparison, Member (NodeTypes g) Literal) => g m Bool
e1 = equality (column PersonAge) (int 1)

spec :: Spec
spec = do
    describe "Column" $ do
        it "restrict context by Reader" $ do
            let proxy = Proxy :: Proxy Person
            pp (hoistExpr (flip runReaderT proxy) e1) `shouldBe` "(`age`==1)"
