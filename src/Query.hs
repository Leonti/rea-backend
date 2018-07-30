{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms   #-}

module Query(runQuery) where

import Db
import Data.Text (Text, unpack)
import Data.Maybe(fromJust)
import Protolude

import GraphQL
import GraphQL.Internal.Syntax.AST (Variable(..))
import GraphQL.Value(makeName)
import GraphQL.Value.ToValue (ToValue(..))
import GraphQL.API
import GraphQL.Resolver (Handler, (:<>)(..))

import qualified Data.Map                  as Map

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
  , Field "timestamp" Double
  ]

type DateCount = Object "DateCount" '[]
  '[Field "date" Text
  , Field "count" Int32
  ]

type Query = Object "Query" '[]
  '[ Field "onSaleProperties" (List OnSaleProperty)
   , Argument "date" Text :> Field "onSalePropertiesForDate" (List OnSaleProperty)
   , Field "onSaleDates" (List DateCount)
   , Field "soldProperties" (List SoldProperty)
   , Argument "date" Text :> Field "soldPropertiesForDate" (List SoldProperty)
   , Field "soldDates" (List DateCount)
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

type SoldProperty = Object "SoldProperty" '[]
  '[Field "distances" (Maybe Distances)
  , Field "geo" (Maybe Geo)
  , Field "link" Text
  , Field "bedrooms" Int32
  , Field "bathrooms" Int32
  , Field "cars" Int32
  , Field "location" Text
  , Field "price" Int32
  , Field "soldAt" Text
  ]

soldPropertyHandler :: Mongo.Document -> Handler IO SoldProperty
soldPropertyHandler doc = pure
  (   pure (distancesHandler <$> Mongo.lookup "distances" doc)
  :<> pure (geoHandler <$> Mongo.lookup "geo" doc)
  :<> pure (fromJust $ Mongo.lookup "link" doc)
  :<> pure (fromJust $ Mongo.lookup "bedrooms" doc)
  :<> pure (fromJust $ Mongo.lookup "bathrooms" doc)
  :<> pure (fromJust $ Mongo.lookup "cars" doc)
  :<> pure (fromJust $ Mongo.lookup "location" doc)
  :<> pure (fromJust $ Mongo.lookup "price" doc)
  :<> pure (fromJust $ Mongo.lookup "soldAt" doc)
  )

propertyListHandler :: Handler IO (List OnSaleProperty)
propertyListHandler =
  fmap onSalePropertyHandler <$> allOnSaleProperties

propertyListForDateHandler :: Text -> Handler IO (List OnSaleProperty)
propertyListForDateHandler date =
  fmap onSalePropertyHandler <$> onSaleNewPropertiesForDate (unpack date)

soldPropertyListForDateHandler :: Text -> Handler IO (List SoldProperty)
soldPropertyListForDateHandler date =
  fmap soldPropertyHandler <$> soldPropertiesForDate (unpack date)

soldPropertyListHandler :: Handler IO (List SoldProperty)
soldPropertyListHandler =
  fmap soldPropertyHandler <$> allSoldProperties

extractCount :: Mongo.Document -> Int32
extractCount doc = fromMaybe 0 (Mongo.lookup "count" doc)

countForDate :: [Mongo.Document] -> Text -> Maybe Int32
countForDate counts date = extractCount <$> find (\count -> Mongo.lookup "_id" count == Just date) counts

onSaleDateHandler :: [Mongo.Document] -> Text -> Handler IO DateCount
onSaleDateHandler counts date = pure
  (   pure date
  :<> pure (fromMaybe 0 (countForDate counts date)))

soldDateCountHandler :: Mongo.Document -> Handler IO DateCount
soldDateCountHandler doc = pure
  (   pure (fromJust $ Mongo.lookup "_id" doc)
  :<> pure (fromJust $ Mongo.lookup "count" doc)
  )

onSaleDatesHandler :: Handler IO (List DateCount)
onSaleDatesHandler = do
  dates <- onSalePropertyDates
  counts <- newPropertiesCounts
  return $ fmap (onSaleDateHandler counts) dates

soldDatesHandler :: Handler IO (List DateCount)
soldDatesHandler = fmap soldDateCountHandler <$> soldPropertyDatesCounts

root :: Handler IO Query
root = pure (   propertyListHandler
              :<> propertyListForDateHandler
              :<> onSaleDatesHandler
              :<> soldPropertyListHandler
              :<> soldPropertyListForDateHandler
              :<> soldDatesHandler
              )
test = vars
  where
    Right varDateName = makeName "date"
    dateVal = toValue @Text "hello"
    vars = Map.singleton (Variable varDateName) dateVal

runQuery :: Text -> VariableValues -> IO Response
runQuery query = interpretQuery @Query root query Nothing
