{-# LANGUAGE OverloadedStrings #-}
module PostgreSQLSpec
    ( spec
    ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Logger (runNoLoggingT)
import Data.Text (Text)
import qualified Data.Text as Text (pack)
import Database.Persist
import Database.Persist.Postgresql
import Test.Hspec

spec :: Spec
spec = do
    describe "PostgreSQL" $ do
        it "test" $ do
            putStrLn "hello"
            runNoLoggingT . runResourceT . withPostgresqlConn connectInfo . runSqlConn $ do
                r <- rawSql (Text.pack $ "SELECT table_name FROM information_schema.tables;") []
                liftIO $ print (r :: [Single Text])
                return ()

connectInfo :: ConnectionString
connectInfo =
    "host=postgres port=5432 user=dbuser password=dbpass dbname=db"
