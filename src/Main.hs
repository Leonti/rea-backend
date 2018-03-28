{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Aeson                as A
import           Data.ByteString.Lazy      (fromStrict)
import qualified Data.ByteString.Lazy      as BS
import qualified Data.CaseInsensitive      as CI
import           Data.Text                 (Text, replace)
import           Data.Word8                (isSpace, toLower)
import           GHC.Generics              (Generic)
import           Network.HTTP.Types
import           Network.HTTP.Types.Header ()
import           Network.Wai
import           Network.Wai.Handler.Warp  (run)
import           Query                     (runQuery)
import           Token                     (AuthConfig (..), verifyJwt)

app :: Application
app request respond =
    case (requestMethod request, pathInfo request) of
        ("POST", ["graphql"]) -> withAuth request (graphQl request) >>= respond
        ("OPTIONS", _) -> optionsResponse >>= respond
        (_, path) -> do
            response <- notFound
            _ <- print $ show path
            respond response

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

badRequest :: IO Response
badRequest = return $ responseLBS
    status400
    corsHeaders
    "Bad request"

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
  let eitherQuery = A.eitherDecode (fromStrict body) :: Either String GraphQlQuery
  case eitherQuery of
    Right q -> do
        print eitherQuery
        response <- A.encode <$> runQuery (replace "query " "" (query q))
        return $ toJsonResponse response
    Left e -> do
        print e
        badRequest

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
