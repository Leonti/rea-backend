{-# LANGUAGE OverloadedStrings #-}
import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)
import qualified Database.MongoDB as Mongo
import Database.MongoDB ((=:))
import System.Environment
import Data.Text
import BsonAeson
import qualified Data.ByteString.Lazy as BS
import qualified Data.Aeson as A

app :: Application
app request respond =
    case pathInfo request of
        x:_ | x == "sold" -> documentsToResponse allSoldProperties >>= respond
        x:date:_ | x == "on-sale" -> documentsToResponse (onSalePropertiesForDate (unpack date)) >>= respond
        x:_ | x == "dates" -> valuesToResponse propertyDates >>= respond
        path -> do
            response <- notFound
            _ <- print $ show path
            respond response

notFound :: IO Response
notFound = return $ responseLBS
    status404
    []
    "Not found"

documentsToResponse :: IO [Mongo.Document] -> IO Response
documentsToResponse = fmap (toJsonResponse . documentsToBS)

valuesToResponse :: IO [Mongo.Value] -> IO Response
valuesToResponse = fmap (toJsonResponse . valuesToBS)

toJsonResponse :: BS.ByteString -> Response
toJsonResponse = responseLBS
    status200
    [("Content-Type", "application/json")]

documentsToBS :: [Mongo.Document] -> BS.ByteString
documentsToBS documents = A.encode (fmap fromDocument documents)

valuesToBS :: [Mongo.Value] -> BS.ByteString
valuesToBS values = A.encode (fmap fromBson values)

actionToIO :: Mongo.Action IO a -> IO a
actionToIO action = do
    pipe <- getAuthenticatedMongoPipe
    mongoDb <- getEnv "MONGO_DB"
    Mongo.access pipe Mongo.UnconfirmedWrites (pack mongoDb) action

allSoldProperties :: IO [Mongo.Document]
allSoldProperties = actionToIO allSoldPropertiesAction

propertyDates :: IO [Mongo.Value]
propertyDates = actionToIO propertyDatesAction

onSalePropertiesForDate :: String -> IO [Mongo.Document]
onSalePropertiesForDate = actionToIO . onSalePropertiesForDateAction

main :: IO ()
main = do
    putStrLn "http://localhost:9050/"
    run 9050 app


allSoldPropertiesAction :: Mongo.Action IO [Mongo.Document]
allSoldPropertiesAction = Mongo.rest =<< Mongo.find (Mongo.select [] "soldProperties")

onSalePropertiesForDateAction :: String -> Mongo.Action IO [Mongo.Document]
onSalePropertiesForDateAction date = Mongo.rest =<< Mongo.find (Mongo.select
    [ "extractedDate" =: date ] "properties")

propertyDatesAction :: Mongo.Action IO [Mongo.Value]
propertyDatesAction = Mongo.distinct "extractedDate" (Mongo.select [] "properties")

--db.getCollection('properties').find({
--    "extractedDate": {
--        $lt: "2017-4-20"
--    },
--    $or: [
--        {link: "/property-apartment-vic-richmond-125257370"}, {link: "/property-apartment-vic-richmond-124707306"}
--    ]
--    })

getAuthenticatedMongoPipe :: IO Mongo.Pipe
getAuthenticatedMongoPipe = do
    mongoHostPort <- getEnv "MONGO_HOST_PORT"
    mongoDb <- getEnv "MONGO_DB"
    mongoUsername <- getEnv "MONGO_USERNAME"
    mongoPassword <- getEnv "MONGO_PASSWORD"
    pipe <- Mongo.connect (Mongo.readHostPort mongoHostPort)
    _ <- Mongo.access pipe Mongo.UnconfirmedWrites (pack mongoDb) $ Mongo.auth (pack mongoUsername) (pack mongoPassword)
    return pipe
