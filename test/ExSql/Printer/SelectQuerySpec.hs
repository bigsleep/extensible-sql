{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
module ExSql.Printer.SelectQuerySpec
    ( spec
    ) where

import qualified Control.Monad.Trans.State.Strict as State (runStateT)
import Data.DList (DList)
import qualified Data.DList as DList
import Data.Functor.Identity (Identity(..))
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Text as Text (unpack)
import qualified Data.Text.Lazy.Builder as TLB
import Database.Persist (DBName(..), Entity(..), PersistEntity(..),
                         PersistValue(..))
import Database.Persist.Class (PersistField(..))
import Database.Persist.Sql (fieldDBName)
import qualified Database.Persist.Sql.Util as Persist (parseEntityValues)
import qualified Database.Persist.TH as Persist (mkPersist, persistLowerCase,
                                                 share, sqlSettings)

import ExSql.Printer.Common
import ExSql.Printer.SelectQuery
import ExSql.Printer.Types
import ExSql.Syntax.Internal.SelectQueryStage
import ExSql.Syntax.Internal.Types
import ExSql.Syntax.Relativity
import ExSql.Syntax.SelectQuery

import Test.Hspec

Persist.share [Persist.mkPersist Persist.sqlSettings] [Persist.persistLowerCase|
Person
    name Text
    age Int
    deriving Show
    deriving Eq
|]

data E a where
    Lit :: (PersistField a) => a -> E a
    Col :: (PersistEntity record) => Ref (Entity record) -> EntityField record a -> E a
    F :: (PersistField a) => Ref a -> E a

pe :: ExprPrinterType E
pe _ _ (Lit a) =
    let v = toPersistValue a
    in StatementBuilder (TLB.singleton '?', return v)
pe _ _ (Col (RelationRef (RRef tid)) col) =
    let columnName = unDBName . fieldDBName $ col
        x = printFromAlias tid `mappend` TLB.singleton '.' `mappend` TLB.fromText columnName
    in StatementBuilder (x, mempty)
pe _ _ (F (FieldRef (FRef fid))) = StatementBuilder (printFieldAlias fid, mempty)
pe _ _ (F (FieldRef (QRef tid fid))) =
    let x = printFromAlias tid `mappend` TLB.singleton '.' `mappend` printFieldAlias fid
    in StatementBuilder (x, mempty)

sq1 :: SelectQuery Neutral E (Entity Person)
sq1 = selectFrom $ \_ _ -> id

sq2 :: SelectQuery FieldsSpecified E (Int, Text, Int)
sq2 = selectFrom $ \(_ :: Ref (Entity Person)) _ -> resultAs ((,,) :$: Sel (Lit 1) :*: Sel (Lit "a") :*: Sel (Lit 2)) $ const id

sq3 :: SelectQuery FieldsSpecified E (Text, Int)
sq3 = selectFrom $ \(ref :: Ref (Entity Person)) _ ->
        resultAs ((,) :$: Sel (Col ref PersonName) :*: Sel (Col ref PersonAge)) $
            \(_ :$: f1 :*: _) -> orderBy (F f1) Asc

sq4 :: SelectQuery FieldsSpecified E (Int, Text)
sq4 = selectFromSub sq3 $ \(_ :$: f1 :*: f2) _ ->
        resultAs ((,) :$: Sel (F f2) :*: Sel (F f1)) $ \(_ :$: f2' :*: f1') -> orderBy (F f1') Asc

sq5 ::  SelectQuery FieldsSpecified E (Entity Person, Int)
sq5 = selectFromSub sq1 $ \(_ :$: sq1ref) sq1alias ->
        resultAs ((,) :$: Star sq1alias :*: Sel (Col sq1ref PersonAge)) $ const id

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

    it "field ref" $ do
        let (convert, r) = renderSelect pe sq3
            expected = mempty
                { scField = Clause . DList.fromList $
                    [ StatementBuilder ("t_0.name AS f_0", mempty)
                    , StatementBuilder ("t_0.age AS f_1", mempty)
                    ]
                , scFrom = Clause . return $ StatementBuilder ("person AS t_0", mempty)
                , scOrderBy = OrderByClause . return $ (StatementBuilder ("f_0", mempty), Asc)
                }
        r `shouldBe` expected
        State.runStateT convert [PersistText "abc", PersistInt64 2] `shouldBe` Right (("abc", 2), [])

    it "sub select from" $ do
        let (convert, r) = renderSelect pe sq4
            expected = mempty
                { scField = Clause . DList.fromList $
                    [ StatementBuilder ("t_0.f_1 AS f_0", mempty)
                    , StatementBuilder ("t_0.f_0 AS f_1", mempty)
                    ]
                , scFrom = Clause . return $ StatementBuilder ("(SELECT t_0.name AS f_0, t_0.age AS f_1 FROM person AS t_0 ORDER BY f_0 ASC) AS t_0", mempty)
                , scOrderBy = OrderByClause . return $ (StatementBuilder ("f_1", mempty), Asc)
                }
        r `shouldBe` expected
        State.runStateT convert [PersistInt64 2, PersistText "abc"]
            `shouldBe` Right ((2, "abc"), [])

    it "sub select from with sub star in select clause" $ do
        let (convert, r) = renderSelect pe sq5
            expected = mempty
                { scField = Clause . DList.fromList $
                    [ StatementBuilder ("t_0.*", mempty)
                    , StatementBuilder ("t_0.age AS f_0", mempty)
                    ]
                , scFrom = Clause . return $ StatementBuilder ("(SELECT  *  FROM person AS t_0) AS t_0", mempty)
                }
        r `shouldBe` expected
        let def = entityDef (Proxy :: Proxy Person)
            Right entity = Persist.parseEntityValues def [PersistInt64 123, PersistText "abc", PersistInt64 2]
        State.runStateT convert [PersistInt64 123, PersistText "abc", PersistInt64 2, PersistInt64 2]
            `shouldBe` Right ((entity, 2), [])
