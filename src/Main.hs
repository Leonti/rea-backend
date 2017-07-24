{-# LANGUAGE OverloadedStrings #-}
import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)
import qualified Database.MongoDB as Mongo
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

toJsonResponse :: BS.ByteString -> Response
toJsonResponse = responseLBS
    status200
    [("Content-Type", "application/json")]

documentsToBS :: [Mongo.Document] -> BS.ByteString
documentsToBS documents = A.encode (fmap fromDocument documents)

actionToIO :: Mongo.Action IO a -> IO a
actionToIO action = do
    pipe <- getAuthenticatedMongoPipe
    mongoDb <- getEnv "MONGO_DB"
    Mongo.access pipe Mongo.UnconfirmedWrites (pack mongoDb) action

allSoldProperties :: IO [Mongo.Document]
allSoldProperties = actionToIO allSoldPropertiesAction

onSalePropertiesForDate :: String -> IO [Mongo.Document]
onSalePropertiesForDate date = actionToIO onSalePropertiesForDateAction

main :: IO ()
main = do
    putStrLn "http://localhost:9050/"
    run 9050 app


allSoldPropertiesAction :: Mongo.Action IO [Mongo.Document]
allSoldPropertiesAction = Mongo.rest =<< Mongo.find (Mongo.select [] "soldProperties")

onSalePropertiesForDateAction :: Mongo.Action IO [Mongo.Document]
onSalePropertiesForDateAction = Mongo.rest =<< Mongo.find (Mongo.select [] "properties")

getAuthenticatedMongoPipe :: IO Mongo.Pipe
getAuthenticatedMongoPipe = do
    mongoHostPort <- getEnv "MONGO_HOST_PORT"
    mongoDb <- getEnv "MONGO_DB"
    mongoUsername <- getEnv "MONGO_USERNAME"
    mongoPassword <- getEnv "MONGO_PASSWORD"
    pipe <- Mongo.connect (Mongo.readHostPort mongoHostPort)
    _ <- Mongo.access pipe Mongo.UnconfirmedWrites (pack mongoDb) $ Mongo.auth (pack mongoUsername) (pack mongoPassword)
    return pipe
