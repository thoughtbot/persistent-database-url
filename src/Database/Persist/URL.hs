{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Database.Persist.URL
    ( fromDatabaseUrl
    ) where

import Control.Monad (MonadPlus, mzero, unless)
import Data.ByteString (ByteString, uncons)
import Data.Monoid ((<>))
import Data.String.Conversions (ConvertibleStrings(..))
import Data.String.Conversions.Monomorphic (toStrictByteString)
import Database.Persist.Postgresql (PostgresConf(..))
import URI.ByteString
    ( Authority(..)
    , Host(..)
    , Port(..)
    , URI(..)
    , UserInfo(..)
    , Scheme(..)
    , parseURI
    , strictURIParserOptions
    )

import qualified Data.ByteString.Char8 as Char8

-- | Build a @'PostgresConf'@ by parsing a database URL String
fromDatabaseUrl
    :: (MonadPlus m, ConvertibleStrings s ByteString)
    => Int -> s -> m PostgresConf
fromDatabaseUrl size url = do
    uri <- abortLeft $ parseURI strictURIParserOptions $ toStrictByteString url
    auth <- abortNothing $ uriAuthority uri
    userInfo <- abortNothing $ authorityUserInfo auth
    port <- abortNothing $ authorityPort auth
    dbName <- abortNothing $ snd <$> uncons (uriPath uri)
    unless (schemeBS (uriScheme uri) == "postgres") mzero

    return PostgresConf
        { pgConnStr =
            "user=" <> uiUsername userInfo
            <> " password=" <> uiPassword userInfo
            <> " host=" <> hostBS (authorityHost auth)
            <> " port=" <> Char8.pack (show $ portNumber port)
            <> " dbname=" <> dbName
        , pgPoolSize = size
        }

abortLeft :: (MonadPlus m, Show e) => Either e b -> m b
abortLeft = either (const mzero) return

abortNothing :: MonadPlus m => Maybe a -> m a
abortNothing = maybe mzero return
