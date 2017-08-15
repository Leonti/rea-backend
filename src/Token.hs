{-# LANGUAGE OverloadedStrings #-}
module Token(verifyJwt, AuthConfig) where

import Crypto.JWT (
    JWKSet(..),
    JWTError,
    stringOrUri,
    defaultJWTValidationSettings,
    decodeCompact,
    verifyClaims,
    claimSub,
    string)

import Data.Aeson (decode, encode, eitherDecode)
import qualified Data.Aeson as A

import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lazy (ByteString)
import Data.Maybe (fromJust)
import Control.Lens (preview, re, review, set, view)
import Control.Monad.Except (runExceptT)
import System.Exit (exitFailure)

import Network.HTTP.Conduit (simpleHttp)
import Data.ByteString.Search (replace)

data AuthConfig = AuthConfig
    { jwksUrl :: String
    , aud :: String
    }

-- "https://leonti.au.auth0.com/.well-known/jwks.json"

verifyJwt :: AuthConfig -> ByteString -> IO (Either String String)
verifyJwt config tokenString = do
  let
    aud' = fromJust $ preview stringOrUri (aud config)
    conf = defaultJWTValidationSettings (== aud')
  jwksString <- jwksFix <$> simpleHttp (jwksUrl config)
  let Just (JWKSet jwks) = decode jwksString :: Maybe JWKSet
  result <- runExceptT
    (decodeCompact tokenString >>= verifyClaims conf (head jwks))
  case result of
    Left e -> return (Left $ show (e :: JWTError))
    Right claims -> return (Right $ fromJust $ view claimSub claims >>= preview string)


-- "https://login.myob.com/discovery/keys"
-- "https://leonti.au.auth0.com/.well-known/jwks.json"

jwksFix :: ByteString -> ByteString
jwksFix = replace (L.toStrict "x5t") (L.toStrict "x5t_unused") . L.toStrict

printJWK :: IO ()
printJWK = do
    jwksStringMyob <- L.readFile "test_jwk_myob.json"
    jwksStringMine <- jwksFix <$> L.readFile "test_jwk.json"
--    jwksHttpString <- simpleHttp "https://login.myob.com/discovery/keys"
    let parsedJwksMyob = decode jwksStringMyob :: Maybe JWKSet
    let parsedJwksMine = decode jwksStringMine :: Maybe JWKSet
    let parsedJwksMineEither = eitherDecode jwksStringMine :: Either String JWKSet
--    _ <- print jwksStringMyob
--    _ <- print jwksStringMine
--    _ <- print parsedJwksMyob
    print parsedJwksMineEither

--verify
