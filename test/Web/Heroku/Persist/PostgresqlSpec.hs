{-# LANGUAGE OverloadedStrings #-}
module Web.Heroku.Persist.PostgresqlSpec (main, spec) where

import Test.Hspec
import Web.Heroku.Persist.Postgresql

import Database.Persist.Postgresql (PostgresConf(..))
import System.Environment (setEnv)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "postgresConf" $
    it "should parse a PostgresConf out of DATABASE_URL" $ do
        setEnv "DATABASE_URL" "postgres://user:password@host:1234/db"

        conf <- postgresConf 10

        pgConnStr conf `shouldBe`
            "user=user password=password host=host port=1234 dbname=db"
        pgPoolSize conf `shouldBe` 10
  describe "fromDatabaseUrl" $
    it "should parse a PostgresConf out of a String" $ do
        let conf = fromDatabaseUrl 10 "postgres://user:password@host:1234/db"

        pgConnStr conf `shouldBe`
            "user=user password=password host=host port=1234 dbname=db"
        pgPoolSize conf `shouldBe` 10
