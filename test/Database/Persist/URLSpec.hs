{-# LANGUAGE OverloadedStrings #-}
module Database.Persist.URLSpec (main, spec) where

import Test.Hspec
import Database.Persist.URL

import Database.Persist.Postgresql (PostgresConf(..))

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "fromDatabaseUrl" $ do
    it "should parse a PostgresConf out of a String" $ do
        let conf = fromDatabaseUrl 10 "postgres://user:password@host:1234/db"

        pgConnStr <$> conf `shouldBe`
            Just "user=user password=password host=host port=1234 dbname=db"
        pgPoolSize <$> conf `shouldBe` Just 10
    it "should handle an invalid URL" $
        fromDatabaseUrl 10 "postgres://user:password@/db"
            `shouldThrow`
                (== userError "DATABASE_URL failed to parse: MalformedPath")
    it "should handle a missing authority" $
        fromDatabaseUrl 10 "postgres:/db"
            `shouldThrow` (== userError "DATABASE_URL is missing authority")
    it "should handle a missing path" $
        fromDatabaseUrl 10 "postgres://user:password@example:123"
            `shouldThrow` (== userError "DATABASE_URL is missing path")
    it "should handle missing authentication" $
        fromDatabaseUrl 10 "postgres://example/db"
            `shouldThrow` (== userError "DATABASE_URL is missing user info")
    it "should handle a different protocol" $
        fromDatabaseUrl 10 "mysql://user:pass@example:123/db"
            `shouldThrow` (== userError "DATABASE_URL has unknown scheme")
