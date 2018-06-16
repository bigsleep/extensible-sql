{-# LANGUAGE OverloadedStrings #-}
module MySQLSpec
    ( spec
    ) where

import qualified Common
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runNoLoggingT)
import Control.Monad.Trans.Resource (runResourceT)
import Data.Text (Text)
import Database.Persist
import Database.Persist.MySQL
import Test.Hspec

spec :: Spec
spec = describe "PostgreSQL" $ do
    before_ (run . runMigration $ Common.migrateAll) $
        Common.tests run

    where
        run = runResourceT . runNoLoggingT . withMySQLConn connectInfo . runSqlConn

connectInfo :: ConnectInfo
connectInfo = defaultConnectInfo
    { connectHost = "mysql"
    , connectPort = 3306
    , connectUser = "dbuser"
    , connectPassword = "dbpass"
    , connectDatabase = "db"
    }
