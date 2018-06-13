{-# LANGUAGE
    DataKinds,
    FlexibleContexts,
    GADTs,
    GeneralizedNewtypeDeriving,
    KindSignatures,
    MultiParamTypeClasses,
    OverloadedStrings,
    QuasiQuotes,
    RankNTypes,
    ScopedTypeVariables,
    TemplateHaskell,
    TypeFamilies,
    TypeOperators
#-}
module Common
    ( migrateAll
    , tests
    , Run
    , Driver(..)
    , Customer(..)
    , Address(..)
    , Journey(..)
    , DriverId
    , CustomerId
    , AddressId
    , JourneyId
    ) where

import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import qualified Control.Monad.Trans.State.Strict as State (evalStateT)
import Control.Monad.Logger (NoLoggingT, runNoLoggingT)
import Data.Conduit (runConduit, (.|))
import qualified Data.Conduit.List as CL
import qualified Data.DList as DList
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Text.Lazy as TL (toStrict)
import qualified Data.Text.Lazy.Builder as TLB (singleton, toLazyText)
import Data.Time (UTCTime)
import qualified Database.Persist as Persist
import qualified Database.Persist.Sql as Persist (SqlBackend, rawExecute, rawQuery, runSqlConn, runSqlPersistM)
import qualified Database.Persist.TH as Persist (mkPersist, mkMigrate, persistLowerCase, share, sqlSettings)

import ExSql.Printer.Types
import ExSql.Printer.SelectQuery
import ExSql.Syntax.Internal.Types
import ExSql.Syntax.SelectQuery

import Test.Hspec

Persist.share [Persist.mkPersist Persist.sqlSettings, Persist.mkMigrate "migrateAll"] [Persist.persistLowerCase|
Driver
    name String
    phoneNumber String
    age Int
    deriving Show
    deriving Eq

Customer
    name String
    phoneNumber String
    frequentAddressId AddressId
    otherDetail
    deriving Show
    deriving Eq

Address
    name String
    building String
    street String
    area String
    townCity String
    deriving Show
    deriving Eq

Journey
    driverId DriverId
    customerId CustomerId
    departurePoint AddressId
    destinationPoint AddressId
    cost Int
    depatureDate UTCTime
    arrivalDate UTCTime
    deriving Show
    deriving Eq
|]

driver1 :: Driver
driver1 = Driver "man" "1234abcd" 51

type Run = forall a. ReaderT Persist.SqlBackend (NoLoggingT (ResourceT IO)) a -> IO a

data E a where
    E :: (Persist.PersistField a) => a -> E a

pe :: ExprPrinterType E
pe _ _ (E a) =
    let v = Persist.toPersistValue a
    in StatementBuilder (TLB.singleton '?', return v)

runSelect :: SelectQuery E a -> ReaderT Persist.SqlBackend (NoLoggingT (ResourceT IO)) (Either Text [a])
runSelect query = do
    let (convert, sc) = renderSelect pe query
        StatementBuilder (tlb, ps) = printSelectClauses sc
        t = TL.toStrict . TLB.toLazyText $ tlb
    xs <- runConduit $ Persist.rawQuery t (DList.toList ps) .| CL.consume
    return . sequence . fmap (State.evalStateT convert) $ xs

testSelect1 :: Run -> Spec
testSelect1 run = it "select1" $ do
    let query = selectFrom $ \(_ :: Ref (Persist.Entity Driver)) -> id
    r <- run $ do
        Persist.rawExecute "delete from driver" []
        _ <- Persist.insert driver1
        runSelect query
    fmap (map Persist.entityVal) r `shouldBe` Right [driver1]

tests :: Run -> Spec
tests run = do
    testSelect1 run
