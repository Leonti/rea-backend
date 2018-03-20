{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Aeson                as A
import qualified Data.Bson                 as B
import           Data.ByteString.Lazy      (fromStrict)
import qualified Data.CaseInsensitive      as CI
import           Data.List
import           Data.Maybe                (fromJust)
import qualified Database.MongoDB          as Mongo
import qualified Data.ByteString.Lazy      as BS
import           Data.Text                 (Text, pack, replace, unpack)
import           Data.Word8                (isSpace, toLower)
import           Query                        (runQuery)
import           GHC.Generics              (Generic)
import           Network.HTTP.Types
import           Network.HTTP.Types.Header()
import           Network.Wai
import           Network.Wai.Handler.Warp  (run)
import           Token                     (AuthConfig (..), verifyJwt)

app :: Application
app request respond =
    case (requestMethod request, pathInfo request) of
--        ("GET", x:_) | x == "sold" -> withAuth request (documentsToResponse allSoldProperties) >>= respond
--        ("GET", [x]) | x == "on-sale" -> withAuth request (documentsToResponse allOnSaleProperties) >>= respond
--        ("GET", x:date:_) | x == "on-sale" -> withAuth request (documentsToResponse (onSalePropertiesForDate (unpack date))) >>= respond
--        ("GET", x:date:_) | x == "new-on-sale" -> withAuth request (documentsToResponse (onSaleNewPropertiesForDate (unpack date))) >>= respond
--        ("GET", x:_) | x == "on-sale-dates" -> withAuth request (valuesToResponse onSalePropertyDates) >>= respond
        ("POST", ["graphql"]) -> graphQl request >>= respond
        ("OPTIONS", _) -> optionsResponse >>= respond
        (_, path) -> do
            response <- notFound
            _ <- print $ show path
            respond response
{-
test :: IO ()
test = do
  docs <- allOnSaleProperties
  response <- A.encode <$> runQuery docs "{ onSaleProperties { location geo { latitude } } }"
  print response
-}

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
  let rawQuery = query $ fromJust maybeQuery
  let replaced = replace "query " "" rawQuery
  print replaced
  response <- A.encode <$> runQuery replaced
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
                    Left _ -> unauthorized
            Nothing -> unauthorized
        Nothing -> unauthorized
{-
documentsToResponse :: IO [Mongo.Document] -> IO Response
documentsToResponse = fmap (toJsonResponse . documentsToBS)

valuesToResponse :: IO [Mongo.Value] -> IO Response
valuesToResponse = fmap (toJsonResponse . valuesToBS)
-}

toJsonResponse :: BS.ByteString -> Response
toJsonResponse = responseLBS
    status200
    (("Content-Type", "application/json") : corsHeaders)

main :: IO ()
main = do
    putStrLn "http://localhost:9050/"
    run 9050 app

extractBearerAuth :: BS.ByteString -> Maybe BS.ByteString
extractBearerAuth bs =
    let (x, y) = BS.break isSpace bs
    in if BS.map toLower x == "bearer"
        then Just $ BS.dropWhile isSpace y
        else Nothing
