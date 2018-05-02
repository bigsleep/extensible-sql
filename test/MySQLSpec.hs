{-# LANGUAGE OverloadedStrings #-}
module MySQLSpec
    ( spec
    ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Logger (runNoLoggingT)
import Data.Text (Text)
import Database.Persist
import Database.Persist.MySQL
import Test.Hspec

spec :: Spec
spec = do
    describe "MySQL" $ do
        it "test1" $ do
            putStrLn "hello"
            runNoLoggingT . runResourceT . withMySQLConn connectInfo . runSqlConn $ do
                r <- rawSql "SHOW DATABASES" []
                liftIO $ print (r :: [Single Text])
                return ()

connectInfo :: ConnectInfo
connectInfo = defaultConnectInfo
    { connectHost = "mysql"
    , connectPort = 3306
    , connectUser = "dbuser"
    , connectPassword = "dbpass"
    , connectDatabase = ""
    }
