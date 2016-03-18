{-# LANGUAGE OverloadedStrings #-}
module Web.Heroku.Persist.Postgresql
    ( postgresConf
    , fromDatabaseUrl
    ) where

import Control.Applicative ((<$>))
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Monoid ((<>))
import Database.Persist.Postgresql (PostgresConf(..))
import Web.Heroku (dbConnParams)
import Web.Heroku.Postgres (dbConnParams, parseDatabaseUrl)

import qualified Data.Text as T

-- | Build a @'PostgresConf'@ by parsing @ENV[DATABASE_URL]@
postgresConf :: Int -> IO PostgresConf
postgresConf size = do
    connStr <- formatParams <$> dbConnParams

    return PostgresConf
        { pgConnStr = connStr
        , pgPoolSize = size
        }

-- | Build a @'PostgresConf'@ by parsing a database URL String
fromDatabaseUrl :: Int -> String -> PostgresConf
fromDatabaseUrl size url = PostgresConf
    { pgConnStr = formatParams $ parseDatabaseUrl url
    , pgPoolSize = size
    }

formatParams :: [(Text, Text)] -> ByteString
formatParams = encodeUtf8 . T.unwords . map toKeyValue

toKeyValue :: (Text, Text) -> Text
toKeyValue (k, v) = k <> "=" <> v
