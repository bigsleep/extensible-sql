{-# LANGUAGE OverloadedStrings, DataKinds, FlexibleContexts, GADTs, GeneralizedNewtypeDeriving, RankNTypes, TypeFamilies, TypeOperators, MultiParamTypeClasses, ScopedTypeVariables, QuasiQuotes, TemplateHaskell #-}
module ExSql.Printer.SelectQuerySpec
    ( spec
    ) where

import qualified Control.Monad.Trans.State.Strict as State (runStateT)
import Data.DList (DList)
import qualified Data.DList as DList
import Data.Text (Text)
import qualified Data.Text as Text (unpack)
import qualified Data.Text.Lazy.Builder as TLB
import Data.Functor.Identity (Identity(..))
import Database.Persist (Entity, PersistValue(..))
import Database.Persist.Class (PersistField(..))
import qualified Database.Persist.TH as Persist (mkPersist, persistLowerCase, share, sqlSettings)

import ExSql.Syntax.SelectQuery
import ExSql.Syntax.Relativity
import ExSql.Syntax.Internal.Types
import ExSql.Printer.SelectQuery
import ExSql.Printer.Types

import Test.Hspec

Persist.share [Persist.mkPersist Persist.sqlSettings] [Persist.persistLowerCase|
Person
    name String
    age Int
    deriving Show
|]

data E a where
    E :: (PersistField a) => a -> E a

pe :: ExprPrinterType E
pe _ _ (E a) =
    let v = toPersistValue a
    in StatementBuilder (TLB.singleton '?', return v)

sq1 :: SelectQuery Identity (Entity Person)
sq1 = selectFrom $ \_ -> id

sq2 :: SelectQuery E (Int, Text, Int)
sq2 = selectFrom $ \(r :: ExSql.Syntax.SelectQuery.Selector Ref (Entity Person)) -> resultAs ((,,) :$ E 1 :* E "a" :* E 2) $ \_ -> id

spec :: Spec
spec = describe "SelectQuery" $ do
    it "from clause" $ do
        let (_, r) = renderSelect undefined sq1
        r `shouldBe` mempty { scFrom = Clause . return $ StatementBuilder ("person AS t_0", mempty) }

    it "field clause" $ do
        let (convert, r) = renderSelect pe sq2
            expected = mempty
                { scField = Clause . DList.fromList $
                    [ StatementBuilder ("? AS f_0", return $ PersistInt64 1)
                    , StatementBuilder ("? AS f_1", return $ PersistText "a")
                    , StatementBuilder ("? AS f_2", return $ PersistInt64 2)
                    ]
                , scFrom = Clause . return $ StatementBuilder ("person AS t_0", mempty)
                }
        r `shouldBe` expected
        State.runStateT convert [PersistInt64 1, PersistText "a", PersistInt64 2] `shouldBe` Right ((1, "a", 2), [])
