{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Query(runQuery) where

import Db
import Data.Text (Text, unpack)
import Data.Maybe(fromJust)
import Protolude

import GraphQL
import GraphQL.API
import GraphQL.Resolver (Handler, (:<>)(..))

import qualified Database.MongoDB          as Mongo

type Geo = Object "Geo" '[]
  '[Field "latitude" Double
  , Field "longitude" Double]

type Distances = Object "Distances" '[]
  '[Field "toColes" Int32
  , Field "toWoolworth" Int32
  , Field "toAldi" Int32
  , Field "toTrain" Int32
  , Field "toTram" Int32
  , Field "toBus" Int32
  ]

type DatePrice = Object "DatePrice" '[]
  '[Field "price" Int32
  , Field "timestamp" Double]

type Query = Object "Query" '[]
  '[ Field "onSaleProperties" (List OnSaleProperty)
   , Argument "date" Text :> Field "onSalePropertiesForDate" (List OnSaleProperty)
   , Field "onSaleDates" (List Text)
   ]

geoHandler :: Mongo.Document -> Handler IO Geo
geoHandler doc = pure
  (   pure (fromJust $ Mongo.lookup "latitude" doc)
  :<> pure (fromJust $ Mongo.lookup "longitude" doc))

distancesHandler :: Mongo.Document -> Handler IO Distances
distancesHandler doc = pure
  (   pure (fromJust $ Mongo.lookup "toColes" doc)
  :<> pure (fromJust $ Mongo.lookup "toWoolworth" doc)
  :<> pure (fromJust $ Mongo.lookup "toAldi" doc)
  :<> pure (fromJust $ Mongo.lookup "toTrain" doc)
  :<> pure (fromJust $ Mongo.lookup "toTram" doc)
  :<> pure (fromJust $ Mongo.lookup "toBus" doc)
  )

datePriceHandler :: Mongo.Document -> Handler IO DatePrice
datePriceHandler doc = pure (priceHandler :<> timestampHandler)
  where
    priceHandler = pure (fromJust $ Mongo.lookup "price" doc)
    timestampHandler = pure (fromJust $ Mongo.lookup "timestamp" doc)

type OnSaleProperty = Object "OnSaleProperty" '[]
  '[Field "distances" (Maybe Distances)
  , Field "geo" (Maybe Geo)
  , Field "link" Text
  , Field "extractedDate" Text
  , Field "extractedAt" Double
  , Field "bedrooms" Int32
  , Field "bathrooms" Int32
  , Field "cars" Int32
  , Field "location" Text
  , Field "datesPrices" (List DatePrice)
  , Field "isSold" Bool
  , Field "salePrice" (Maybe Int32)
  , Field "soldAt" (Maybe Text)
  ]
onSalePropertyHandler :: Mongo.Document -> Handler IO OnSaleProperty
onSalePropertyHandler doc = pure
  (   pure (distancesHandler <$> Mongo.lookup "distances" doc)
  :<> pure (geoHandler <$> Mongo.lookup "geo" doc)
  :<> pure (fromJust $ Mongo.lookup "link" doc)
  :<> pure (fromJust $ Mongo.lookup "extractedDate" doc)
  :<> pure (fromJust $ Mongo.lookup "extractedAt" doc)
  :<> pure (fromJust $ Mongo.lookup "bedrooms" doc)
  :<> pure (fromJust $ Mongo.lookup "bathrooms" doc)
  :<> pure (fromJust $ Mongo.lookup "cars" doc)
  :<> pure (fromJust $ Mongo.lookup "location" doc)
  :<> pure (datePriceHandler <$> fromJust (Mongo.lookup "datesPrices" doc))
  :<> pure (fromJust $ Mongo.lookup "isSold" doc)
  :<> pure (pure <$> Mongo.lookup "salePrice" doc)
  :<> pure (pure <$> Mongo.lookup "soldAt" doc)
  )

propertyListHandler :: Handler IO (List OnSaleProperty)
propertyListHandler = do
  docs <- allOnSaleProperties
  pure (fmap onSalePropertyHandler docs)

propertyListForDateHandler :: Text -> Handler IO (List OnSaleProperty)
propertyListForDateHandler date = do
  docs <- onSaleNewPropertiesForDate (unpack date)
  pure (fmap onSalePropertyHandler docs)  

onSaleDatesHandler :: Handler IO (List Text)
onSaleDatesHandler = do
  dates <- onSalePropertyDates
  pure (fmap pure dates)

root :: Handler IO Query
root = pure (   propertyListHandler
              :<> propertyListForDateHandler
              :<> onSaleDatesHandler
              )          

runQuery :: Text -> IO Response
runQuery = interpretAnonymousQuery @Query root