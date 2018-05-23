{-# LANGUAGE OverloadedStrings, DataKinds, FlexibleContexts, GADTs, GeneralizedNewtypeDeriving, KindSignatures, MultiParamTypeClasses, RankNTypes, TypeFamilies, TypeOperators, QuasiQuotes, TemplateHaskell #-}
module Common
    ( migrateAll
    , Driver(..)
    , Customer(..)
    , Address(..)
    , Journey(..)
    ) where

import Data.Time (UTCTime)
import qualified Database.Persist.TH as Persist (mkPersist, mkMigrate, persistLowerCase, share, sqlSettings)

Persist.share [Persist.mkPersist Persist.sqlSettings, Persist.mkMigrate "migrateAll"] [Persist.persistLowerCase|
Driver
    name String
    phoneNumber String
    age Int

Customer
    name String
    phoneNumber String
    frequentAddressId AddressId
    otherDetail

Address
    name String
    building String
    street String
    area String
    townCity String

Journey
    driverId DriverId
    customerId CustomerId
    departurePoint AddressId
    destinationPoint AddressId
    cost Int
    depatureDate UTCTime
    arrivalDate UTCTime
|]
