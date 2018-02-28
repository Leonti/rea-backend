{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
import           BsonAeson
import qualified Data.Aeson                as A
import qualified Data.Bson                 as B
import           Data.ByteString.Lazy      (fromStrict)
import qualified Data.ByteString.Lazy      as BS
import qualified Data.CaseInsensitive      as CI
import           Data.List
import           Data.Maybe                (fromJust)
import           Data.Text                 (Text, pack, replace, unpack)
import           Data.Text.Encoding        (decodeUtf8)
import           Data.Word8                (isSpace, toLower)
import           Database.MongoDB          ((=:))
import qualified Database.MongoDB          as Mongo
import           Db                        (runQuery)
import           GHC.Generics              (Generic)
import           Network.HTTP.Types
import qualified Network.HTTP.Types.Header as Header
import           Network.Wai
import           Network.Wai.Handler.Warp  (run)
import           System.Environment
import           Token                     (AuthConfig (..), verifyJwt)

app :: Application
app request respond =
    case (requestMethod request, pathInfo request) of
        ("GET", x:_) | x == "sold" -> withAuth request (documentsToResponse allSoldProperties) >>= respond
        ("GET", [x]) | x == "on-sale" -> withAuth request (documentsToResponse allOnSaleProperties) >>= respond
        ("GET", x:date:_) | x == "on-sale" -> withAuth request (documentsToResponse (onSalePropertiesForDate (unpack date))) >>= respond
        ("GET", x:date:_) | x == "new-on-sale" -> withAuth request (documentsToResponse (onSaleNewPropertiesForDate (unpack date))) >>= respond
        ("GET", x:_) | x == "on-sale-dates" -> withAuth request (valuesToResponse onSalePropertyDates) >>= respond
        ("POST", ["graphql"]) -> graphQl request >>= respond
        ("OPTIONS", _) -> optionsResponse >>= respond
        (_, path) -> do
            response <- notFound
            _ <- print $ show path
            respond response

test :: IO ()
test = do
  docs <- allOnSaleProperties
  response <- A.encode <$> runQuery docs "{ onSaleProperties { location geo { latitude } } }"
  print response

notFound :: IO Response
notFound = return $ responseLBS
    status404
    []
    "Not found"

corsHeaders :: [Header]
corsHeaders =
  [ (CI.mk "Access-Control-Allow-Methods", "OPTIONS, GET, POST")
  , (CI.mk "Access-Control-Allow-Origin", "*")
  , (CI.mk "Access-Control-Allow-Headers", "authorization, content-type")
  ]

optionsResponse :: IO Response
optionsResponse = return $ responseLBS
    status200
    corsHeaders
    ""

unauthorized :: IO Response
unauthorized = return $ responseLBS
    status401
    corsHeaders
    "Unauthorized"

allowedSubs :: [String]
allowedSubs = ["google-oauth2|106496533429095347725"]

authConfig :: AuthConfig
authConfig = AuthConfig
            { jwksUrl = "https://leonti.au.auth0.com/.well-known/jwks.json"
            , aud = "rea-backend"
            }

newtype GraphQlQuery = GraphQlQuery
  { query :: Text
  } deriving (Show, Generic, A.FromJSON)

graphQl :: Request -> IO Response
graphQl request = do
  body <- requestBody request
  let maybeQuery = A.decode (fromStrict body) :: Maybe GraphQlQuery
  docs <- allOnSaleProperties
  let rawQuery = query $ fromJust maybeQuery
  let replaced = replace "query " "" rawQuery
  print replaced
  response <- A.encode <$> runQuery docs replaced
  return $ toJsonResponse response

withAuth :: Request -> IO Response -> IO Response
withAuth request authorizedResponse =
    case Prelude.lookup hAuthorization (requestHeaders request) of
        Just authorization -> case extractBearerAuth $ fromStrict authorization of
            Just token -> do
                subEither <- verifyJwt authConfig token
                case subEither of
                    Right sub -> if sub `elem` allowedSubs then
                            authorizedResponse
                        else
                            unauthorized
                    Left e -> unauthorized
            Nothing -> unauthorized
        Nothing -> unauthorized

documentsToResponse :: IO [Mongo.Document] -> IO Response
documentsToResponse = fmap (toJsonResponse . documentsToBS)

valuesToResponse :: IO [Mongo.Value] -> IO Response
valuesToResponse = fmap (toJsonResponse . valuesToBS)

toJsonResponse :: BS.ByteString -> Response
toJsonResponse = responseLBS
    status200
    (("Content-Type", "application/json") : corsHeaders)

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

allOnSaleProperties :: IO [Mongo.Document]
allOnSaleProperties = actionToIO allOnSalePropertiesAction

onSalePropertyDates :: IO [Mongo.Value]
onSalePropertyDates = actionToIO onSalePropertyDatesAction

onSalePropertiesForDate :: String -> IO [Mongo.Document]
onSalePropertiesForDate = actionToIO . onSalePropertiesForDateAction

onSaleNewPropertiesForDate :: String -> IO [Mongo.Document]
onSaleNewPropertiesForDate = actionToIO . onSaleNewPropertiesForDateAction

main :: IO ()
main = do
    putStrLn "http://localhost:9050/"
    run 9050 app


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
    let linksForTheDate = fmap (extractField "link") propertiesForDate
    existingProperties <- onSaleExistingPropertiesBeforeDateAction date linksForTheDate
    let existingLinks = fmap (extractField "link") existingProperties
    let newLinks = linksForTheDate \\ existingLinks
    let newProperties = Data.List.filter (\p -> extractField "link" p `Data.List.elem` newLinks) propertiesForDate
    return newProperties

extractField :: String -> B.Document -> String
extractField label doc = fieldToString $ B.valueAt (pack label) doc

fieldToString :: B.Value -> String
fieldToString (B.String s) = unpack s
fieldToString _            = error "Value is not a string"

--db.getCollection('properties').find({
--    "extractedDate": {
--        $lt: "2017-4-20"
--    },
--    $or: [
--        {link: "/property-apartment-vic-richmond-125257370"}, {link: "/property-apartment-vic-richmond-124707306"}
--    ]
--    }, {link: 1, _id: 0})

getAuthenticatedMongoPipe :: IO Mongo.Pipe
getAuthenticatedMongoPipe = do
    mongoHostPort <- getEnv "MONGO_HOST_PORT"
    mongoDb <- getEnv "MONGO_DB"
    mongoUsername <- getEnv "MONGO_USERNAME"
    mongoPassword <- getEnv "MONGO_PASSWORD"
    pipe <- Mongo.connect (Mongo.readHostPort mongoHostPort)
    _ <- Mongo.access pipe Mongo.UnconfirmedWrites (pack mongoDb) $ Mongo.auth (pack mongoUsername) (pack mongoPassword)
    return pipe

extractBearerAuth :: BS.ByteString -> Maybe BS.ByteString
extractBearerAuth bs =
    let (x, y) = BS.break isSpace bs
    in if BS.map toLower x == "bearer"
        then Just $ BS.dropWhile isSpace y
        else Nothing
