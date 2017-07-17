{-# LANGUAGE OverloadedStrings #-}

module BsonAeson where

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AT
import qualified Data.Bson as B
import qualified Data.Scientific as Scientific
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as M
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Int

import Data.Bson ((=:))

import Control.Monad (mzero)
import Data.Maybe

import Database.MongoDB
import Control.Monad.Trans (liftIO)

--instance B.Val B.Value where
--    val   = id
--    cast' = Just

toBson :: A.Value -> B.Value
toBson (A.String s)         = B.String s
toBson (A.Number n)
   | exponent < 0                              = Float (Scientific.toRealFloat n :: Double)
   | int64MinBound <= n && n <  int32MinBound  = Int64 $ fromIntegral coefficient * 10 ^ exponent
   | int32MinBound <= n && n <= int32MaxBound  = Int32 $ fromIntegral coefficient * 10 ^ exponent
   | int32MaxBound <  n && n <= int64MaxBound  = Int64 $ fromIntegral coefficient * 10 ^ exponent
   | otherwise                                 = error $ "Integer out of range: " ++ show n
     where
       exponent       = Scientific.base10Exponent n
       coefficient    = Scientific.coefficient n
       int64MaxBound  = toScientific (maxBound :: Int64)
       int32MaxBound  = toScientific (maxBound :: Int32)
       int64MinBound  = toScientific (minBound :: Int64)
       int32MinBound  = toScientific (minBound :: Int32)
       toScientific i = Scientific.scientific (fromIntegral i :: Integer ) 0
toBson (A.Bool b)           = B.Bool b
toBson (A.Array a)          = B.Array $ map toBson (V.toList a)
toBson (A.Object o)         = B.Doc $ map (\(k, v) -> k =: toBson v) (M.toList o)
toBson  A.Null              = B.Null


fromBson :: B.Value -> A.Value
fromBson (B.Float f)   = A.toJSON f
fromBson (B.String s)  = A.String s
fromBson (B.Doc d)     = A.object $ map fieldToPair d
fromBson (B.Array a)   = A.Array . V.fromList $ map fromBson a
fromBson (B.ObjId n)   = A.String . T.pack $ show n
fromBson (B.Bool b)    = A.Bool b
fromBson (B.UTC t)     = A.String . T.pack $ show t
fromBson (B.Int32 n)   = A.toJSON n
fromBson (B.Int64 n)   = A.toJSON n
fromBson (B.Uuid u)    = A.String . T.pack $ show u
fromBson (B.RegEx r)   = A.String . T.pack $ show r
fromBson  B.Null       = A.Null
-- discard these BSON values
fromBson (B.Bin _)     = A.Null
fromBson (B.Fun _)     = A.Null
fromBson (B.Md5 _)     = A.Null
fromBson (B.UserDef _) = A.Null
fromBson (B.Stamp _)   = A.Null
fromBson (B.MinMax _)  = A.Null
fromBson (B.JavaScr _) = A.Null
fromBson (B.Sym _)     = A.Null

fieldToPair :: B.Field -> AT.Pair
fieldToPair f = (B.label f, fromBson (B.value f))

fromDocument :: B.Document -> A.Value
fromDocument d = A.object $ map fieldToPair d
