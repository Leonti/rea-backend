{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
module QueryParser(GraphQlQuery(..), decodeQuery) where

import qualified Data.Aeson                  as A
import qualified Data.Aeson.Types            as AT
import qualified Data.ByteString.Lazy        as BS
import           Data.HashMap.Strict
import           Data.Int
import qualified Data.Map                    as Map
import qualified Data.Scientific             as Scientific
import           Data.Text                   (Text)
import qualified Data.Vector                 as V
import           GraphQL                     (Value, VariableValues)
import qualified GraphQL.Internal.Syntax.AST as AST
import           GraphQL.Value               (Name, NameError,
                                              pattern ValueNull, makeName)
import           GraphQL.Value.ToValue       (ToValue (..))

data GraphQlQuery = GraphQlQuery
  { query          :: Text
  , variableValues :: VariableValues
  } deriving (Show)

-- let eitherQuery = A.eitherDecode (fromStrict body) :: Either String GraphQlQuery

instance AT.FromJSON GraphQlQuery where
  parseJSON = AT.withObject "graphQlQuery" $ \o -> do
    q <- o AT..: "query"
    variables <- o AT..:? "variables"
    case maybe (Right mempty) toVariableValues variables of
      Left e               -> fail "Failed to parse variable values"
      Right variableValues -> return $ GraphQlQuery q variableValues

decodeQuery :: BS.ByteString -> Either String GraphQlQuery
decodeQuery = A.eitherDecode

toVarValue :: AT.Value -> Value
toVarValue (AT.String v) = toValue v
toVarValue (AT.Number n)
    | exponent < 0                              = toValue (Scientific.toRealFloat n :: Double)
    | int32MinBound <= n && n <= int32MaxBound  = toValue ((fromIntegral coefficient * 10 ^ exponent) :: Int32)
    | otherwise                                 = error $ "Integer out of range: " ++ show n
      where
        exponent       = Scientific.base10Exponent n
        coefficient    = Scientific.coefficient n
        int32MaxBound  = toScientific (maxBound :: Int32)
        int32MinBound  = toScientific (minBound :: Int32)
        toScientific i = Scientific.scientific (fromIntegral i :: Integer ) 0
toVarValue (AT.Bool v)   = toValue v
toVarValue (AT.Array v)  = toValue (fmap toVarValue (V.toList v))
toVarValue (AT.Object v) = undefined
toVarValue AT.Null       = ValueNull

toVariableValue :: (Text, AT.Value) -> Either NameError (AST.Variable, Value)
toVariableValue (name, v) = fmap (\n -> (AST.Variable n, toVarValue v)) (makeName name)

toVariableValues :: AT.Object -> Either NameError VariableValues
toVariableValues o = Map.fromList <$> e
  where
    l = toList o
    e = traverse toVariableValue l
