{-# LANGUAGE OverloadedStrings #-}
module PostgreSQLSpec
    ( spec
    ) where

import qualified Common
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Logger (runNoLoggingT)
import Data.Text (Text)
import qualified Data.Text as Text (pack)
import Database.Persist
import Database.Persist.Postgresql
import Test.Hspec

spec :: Spec
spec = describe "PostgreSQL" $
    before_ (run . runMigration $ Common.migrateAll) $ Common.tests run

    where
        run = runResourceT . runNoLoggingT . withPostgresqlConn connectInfo . runSqlConn

connectInfo :: ConnectionString
connectInfo =
    "host=postgres port=5432 user=dbuser password=dbpass dbname=db"
