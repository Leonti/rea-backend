{-# LANGUAGE OverloadedStrings #-}
module Db where

import           BsonAeson
import qualified Data.Aeson           as A
import qualified Data.ByteString.Lazy as BS
import           Data.List
import           Data.Maybe           (fromJust)
import           Data.Text            (Text, pack)
import           Database.MongoDB     ((=:))
import qualified Database.MongoDB     as Mongo
import           System.Environment   (getEnv)

allOnSaleProperties :: IO [Mongo.Document]
allOnSaleProperties = actionToIO allOnSalePropertiesAction

allSoldProperties :: IO [Mongo.Document]
allSoldProperties = actionToIO allSoldPropertiesAction

onSalePropertyDates :: IO [Text]
onSalePropertyDates =  fmap (\x -> textValue <$> x) (actionToIO onSalePropertyDatesAction)

textValue :: Mongo.Value -> Text
textValue v = fromJust (Mongo.cast' v :: Maybe Text)

onSalePropertiesForDate :: String -> IO [Mongo.Document]
onSalePropertiesForDate = actionToIO . onSalePropertiesForDateAction

onSaleNewPropertiesForDate :: String -> IO [Mongo.Document]
onSaleNewPropertiesForDate = actionToIO . onSaleNewPropertiesForDateAction

newPropertiesCounts :: IO [Mongo.Document]
newPropertiesCounts = actionToIO newPropertiesCountsAction

allSoldPropertiesAction :: Mongo.Action IO [Mongo.Document]
allSoldPropertiesAction = Mongo.rest =<< Mongo.find (Mongo.select [] "processedSoldProperties")

allOnSalePropertiesAction :: Mongo.Action IO [Mongo.Document]
allOnSalePropertiesAction = Mongo.rest =<< Mongo.find (Mongo.select [] "processedOnSaleProperties")

onSalePropertiesForDateAction :: String -> Mongo.Action IO [Mongo.Document]
onSalePropertiesForDateAction date = Mongo.rest =<< Mongo.find (Mongo.select
    [ "extractedDate" =: date ] "processedOnSaleProperties")

onSalePropertyDatesAction :: Mongo.Action IO [Mongo.Value]
onSalePropertyDatesAction = Mongo.distinct "extractedDate" (Mongo.select [] "properties")

newPropertiesCountsAction :: Mongo.Action IO [Mongo.Document]
newPropertiesCountsAction = Mongo.aggregate "processedOnSaleProperties"
  [["$group" =: ["_id" =: ("$firstOnSale" :: String), "count" =: ["$sum" =: (1 :: Int)]]]
  ]

-- db.getCollection('processedOnSaleProperties').find({extractedDate: "2018-4-8", $where: "this.datesPrices.length == 2"})
onSaleExistingPropertiesBeforeDateAction :: String -> [String] -> Mongo.Action IO [Mongo.Document]
onSaleExistingPropertiesBeforeDateAction date links = Mongo.rest =<< Mongo.find (Mongo.select
    [ "extractedDate" =: ["$lt" =: date]
    , "$or" =: fmap (\link -> ["link" =: link]) links
     ] "processedOnSaleProperties") {Mongo.project = ["link" =: (1 :: Int), "_id" =: (0 :: Int)]}

onSaleNewPropertiesForDateAction :: String -> Mongo.Action IO [Mongo.Document]
onSaleNewPropertiesForDateAction date =  Mongo.rest =<< Mongo.find (Mongo.select
    [ "firstOnSale" =: date ] "processedOnSaleProperties")

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
