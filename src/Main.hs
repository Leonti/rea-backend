{-# LANGUAGE OverloadedStrings #-}
import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)
import qualified Database.MongoDB as Mongo
import System.Environment
import Data.Text
import BsonAeson
import qualified Data.Aeson as A

app :: Application
app request respond = do
    let path = pathInfo request
    _ <- putStr $ show path
    pipe <- getAuthenticatedMongoPipe
    mongoDb <- getEnv "MONGO_DB"
    properties <- Mongo.access pipe Mongo.UnconfirmedWrites (pack mongoDb) allSoldProperties
    let jsonProperties = fmap fromDocument properties
    let asByteString = A.encode jsonProperties
    putStrLn "I've done some IO here"
    respond $ responseLBS
        status200
        [("Content-Type", "application/json")]
        asByteString

main :: IO ()
main = do
    putStrLn "http://localhost:9050/"
    run 9050 app



allSoldProperties :: Mongo.Action IO [Mongo.Document]
allSoldProperties = Mongo.rest =<< Mongo.find (Mongo.select [] "soldProperties")

getAuthenticatedMongoPipe :: IO Mongo.Pipe
getAuthenticatedMongoPipe = do
    mongoHostPort <- getEnv "MONGO_HOST_PORT"
    mongoDb <- getEnv "MONGO_DB"
    mongoUsername <- getEnv "MONGO_USERNAME"
    mongoPassword <- getEnv "MONGO_PASSWORD"
    pipe <- Mongo.connect (Mongo.readHostPort mongoHostPort)
    _ <- Mongo.access pipe Mongo.UnconfirmedWrites (pack mongoDb) $ Mongo.auth (pack mongoUsername) (pack mongoPassword)
    return pipe
