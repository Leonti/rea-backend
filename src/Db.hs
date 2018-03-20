{-# LANGUAGE OverloadedStrings #-}
module Db where

import           BsonAeson
import           Database.MongoDB          ((=:))
import qualified Database.MongoDB          as Mongo
import qualified Data.ByteString.Lazy      as BS
import qualified Data.Aeson                as A
import           System.Environment(getEnv)
import           Data.Text                 (Text, pack, replace, unpack)
import           Data.List

allOnSaleProperties :: IO [Mongo.Document]
allOnSaleProperties = actionToIO allOnSalePropertiesAction

allSoldProperties :: IO [Mongo.Document]
allSoldProperties = actionToIO allSoldPropertiesAction

onSalePropertyDates :: IO [Mongo.Value]
onSalePropertyDates = actionToIO onSalePropertyDatesAction

onSalePropertiesForDate :: String -> IO [Mongo.Document]
onSalePropertiesForDate = actionToIO . onSalePropertiesForDateAction

onSaleNewPropertiesForDate :: String -> IO [Mongo.Document]
onSaleNewPropertiesForDate = actionToIO . onSaleNewPropertiesForDateAction   


allSoldPropertiesAction :: Mongo.Action IO [Mongo.Document]
allSoldPropertiesAction = Mongo.rest =<< Mongo.find (Mongo.select [] "processedSoldProperties")

allOnSalePropertiesAction :: Mongo.Action IO [Mongo.Document]
allOnSalePropertiesAction = Mongo.rest =<< Mongo.find (Mongo.select [] "processedOnSaleProperties")

onSalePropertiesForDateAction :: String -> Mongo.Action IO [Mongo.Document]
onSalePropertiesForDateAction date = Mongo.rest =<< Mongo.find (Mongo.select
    [ "extractedDate" =: date ] "properties")

onSalePropertyDatesAction :: Mongo.Action IO [Mongo.Value]
onSalePropertyDatesAction = Mongo.distinct "extractedDate" (Mongo.select [] "properties")

onSaleExistingPropertiesBeforeDateAction :: String -> [String] -> Mongo.Action IO [Mongo.Document]
onSaleExistingPropertiesBeforeDateAction date links = Mongo.rest =<< Mongo.find (Mongo.select
    [ "extractedDate" =: ["$lt" =: date]
    , "$or" =: fmap (\link -> ["link" =: link]) links
     ] "properties") {Mongo.project = ["link" =: (1 :: Int), "_id" =: (0 :: Int)]}

onSaleNewPropertiesForDateAction :: String -> Mongo.Action IO [Mongo.Document]
onSaleNewPropertiesForDateAction date = do
    propertiesForDate <- onSalePropertiesForDateAction date
    let linksForTheDate = fmap (Mongo.lookup "link") propertiesForDate
    existingProperties <- onSaleExistingPropertiesBeforeDateAction date linksForTheDate
    let existingLinks = fmap (Mongo.lookup "link") existingProperties
    let newLinks = linksForTheDate \\ existingLinks
    let newProperties = Data.List.filter (\p -> Mongo.lookup "link" p `Data.List.elem` newLinks) propertiesForDate
    return newProperties

documentsToBS :: [Mongo.Document] -> BS.ByteString
documentsToBS documents = A.encode (fmap fromDocument documents)

valuesToBS :: [Mongo.Value] -> BS.ByteString
valuesToBS values = A.encode (fmap fromBson values)

actionToIO :: Mongo.Action IO a -> IO a
actionToIO action = do
    pipe <- getAuthenticatedMongoPipe
    mongoDb <- getEnv "MONGO_DB"
    Mongo.access pipe Mongo.UnconfirmedWrites (pack mongoDb) action

getAuthenticatedMongoPipe :: IO Mongo.Pipe
getAuthenticatedMongoPipe = do
    mongoHostPort <- getEnv "MONGO_HOST_PORT"
    mongoDb <- getEnv "MONGO_DB"
    mongoUsername <- getEnv "MONGO_USERNAME"
    mongoPassword <- getEnv "MONGO_PASSWORD"
    pipe <- Mongo.connect (Mongo.readHostPort mongoHostPort)
    _ <- Mongo.access pipe Mongo.UnconfirmedWrites (pack mongoDb) $ Mongo.auth (pack mongoUsername) (pack mongoPassword)
    return pipe    