{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Db(runQuery) where

import Data.Text (Text, pack)
import Data.Maybe(fromJust)
import Data.Monoid ((<>))
import Protolude

import GraphQL
import GraphQL.API
import GraphQL.Resolver (Handler, (:<>)(..))

import           Database.MongoDB          ((=:))
import qualified Database.MongoDB          as Mongo


type Hello = Object "Hello" '[]
  '[ Argument "who" Text :> Field "greeting" Text ]

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

type OnSalePropertyList = Object "OnSalePropertyList" '[]
  '[Field "onSaleProperties" (List OnSaleProperty)]

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

onSalePropertyListHandler :: [Mongo.Document] -> Handler IO OnSalePropertyList
onSalePropertyListHandler docs = pure $ do
  pure $ fmap onSalePropertyHandler docs

hello :: Handler IO Hello
hello = pure (\who -> pure ("Hello " <> who))

runHelloQuery :: Text -> IO Response
runHelloQuery = interpretAnonymousQuery @Hello hello

runQuery :: [Mongo.Document] -> Text -> IO Response
runQuery docs = interpretAnonymousQuery @OnSalePropertyList (onSalePropertyListHandler docs)
