{-# LANGUAGE OverloadedStrings, DataKinds, FlexibleContexts, GADTs, GeneralizedNewtypeDeriving, RankNTypes, TypeFamilies, TypeOperators, MultiParamTypeClasses, ScopedTypeVariables, QuasiQuotes, TemplateHaskell #-}
module ExSql.Printer.SelectQuerySpec
    ( spec
    ) where

import Data.DList (DList)
import qualified Data.DList as DList
import Data.Text (Text)
import qualified Data.Text as Text (unpack)
import Data.Functor.Identity (Identity(..))
import Database.Persist (Entity, PersistValue(..))
import Database.Persist.Class (PersistField(..))
import qualified Database.Persist.TH as Persist (mkPersist, persistLowerCase, share, sqlSettings)

import ExSql.Syntax.SelectQuery
import ExSql.Syntax.Relativity
import ExSql.Syntax.Internal.Types
import ExSql.Printer.SelectQuery

import Test.Hspec

Persist.share [Persist.mkPersist Persist.sqlSettings] [Persist.persistLowerCase|
Person
    name String
    age Int
    deriving Show
|]

data E a where
    E :: (PersistField a) => a -> E a

pe :: Maybe Relativity -> Maybe Relativity -> E a -> (Text, DList PersistValue)
pe _ _ (E a) =
    let v = toPersistValue a
    in ("?", return v)

sq1 :: SelectQuery Identity (Entity Person)
sq1 = selectFrom $ \_ -> id

sq2 :: SelectQuery E (Int, Text, Int)
sq2 = selectFrom $ \(r :: Ref Person) -> resultAs ((,,) :$ E 1 :* E "a" :* E 2) $ \_ -> id

spec :: Spec
spec = describe "SelectQuery" $ do
    it "from clause" $ do
        let (_, r) = renderSelect undefined sq1
        r `shouldBe` mempty { scFrom = Clause . return $ ("person", mempty) }

    it "field clause" $ do
        let (convert, r) = renderSelect pe sq2
            expected = mempty
                { scField = Clause . DList.fromList $ [("?", return $ PersistInt64 1), ("?", return $ PersistText "a"), ("?", return $ PersistInt64 2)]
                , scFrom = Clause . return $ ("person", mempty)
                }
        r `shouldBe` expected
        convert [PersistInt64 1, PersistText "a", PersistInt64 2] `shouldBe` Right (1, "a", 2)
