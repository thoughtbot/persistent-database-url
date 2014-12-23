{-# LANGUAGE OverloadedStrings #-}
module Web.Heroku.Persist.Postgresql
    ( postgresConf
    ) where

import Control.Applicative ((<$>))
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Database.Persist.Postgresql (PostgresConf(..))
import Web.Heroku (dbConnParams)

import qualified Data.Text as T

-- | Build a @'PostgresConf'@ by parsing @DATABASE_URL@
postgresConf :: Int -> IO PostgresConf
postgresConf size = do
    connStr <- formatParams <$> dbConnParams

    return PostgresConf
        { pgConnStr = connStr
        , pgPoolSize = size
        }

formatParams :: [(Text, Text)] -> ByteString
formatParams = encodeUtf8 . T.unwords . map toKeyValue

toKeyValue :: (Text, Text) -> Text
toKeyValue (k, v) = k `T.append` "=" `T.append` v
