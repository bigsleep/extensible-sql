{-# LANGUAGE DataKinds, FlexibleContexts, GADTs, GeneralizedNewtypeDeriving, KindSignatures, MultiParamTypeClasses, RankNTypes, TypeFamilies, TypeOperators, QuasiQuotes, TemplateHaskell #-}
module ExSql.Syntax.SelectSpec
    ( spec
    ) where

import Control.Monad.Reader.Class (MonadReader(..))
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Data.Extensible (Member, Match(..), (:|)(..), (:*), (<:), nil, hindex)
import Data.Functor.Identity (Identity(..))
import Data.Proxy (Proxy(..))
import qualified Data.Text as Text (unpack)
import qualified Database.Persist as Persist (EntityDef(..), PersistEntity(..), DBName(..))
import qualified Database.Persist.Sql as Persist (fieldDBName)
import qualified Database.Persist.TH as Persist (mkPersist, persistLowerCase, share, sqlSettings)
import ExSql.Syntax.Column
import ExSql.Syntax.Comparison
import ExSql.Syntax.Class
import ExSql.Syntax.Literal
import ExSql.Syntax.Select
import Test.Hspec

Persist.share [Persist.mkPersist Persist.sqlSettings] [Persist.persistLowerCase|
Person
    name String
    age Int
    deriving Show
|]

newtype Printer (g :: * -> *) (v :: (* -> *) -> * -> *) = Printer { runPrinter :: forall a. v g a -> String }
type Nodes = '[Select, Column, Comparison, Literal]
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

ppSelect :: (forall b. Expr xs Identity b -> String) -> Select (Expr xs Identity) a -> String
ppSelect p (Select target cond) = "(SELECT * FROM `" `mappend` table `mappend` "` WHERE " `mappend` p cond `mappend` ")"
    where
    table = Text.unpack . Persist.unDBName . Persist.entityDB . Persist.entityDef $ target

printers :: (forall b. Expr Nodes Identity b -> String) -> Printers Nodes a
printers p = Printer (ppSelect p)
    <: Printer ppColumn
    <: Printer (ppComparison p)
    <: Printer ppLiteral
    <: nil

runPrinters :: Printer (Expr xs Identity) :* xs -> Expr xs Identity a -> String
runPrinters printers (Expr (EmbedAt p (Node (Identity a)))) = runPrinter (hindex printers p) a

pp :: Expr Nodes Identity a -> String
pp = runPrinters (printers pp)

e1 :: (Ast g, xs ~ NodeTypes g, MonadReader (Proxy Person) m, Member xs Select, Member xs Column, Member xs Comparison, Member xs Literal) => g m Person
e1 = select (Proxy :: Proxy Person) (equality (column PersonAge) (int 1))

spec :: Spec
spec = do
    describe "Select" $ do
        it "select" $ do
            let proxy = Proxy :: Proxy Person
            pp (hoistExpr (flip runReaderT proxy) e1) `shouldBe` "(SELECT * FROM `person` WHERE (`age`==1))"
