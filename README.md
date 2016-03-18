# heroku-persistent

A thin wrapper over the [heroku][] package. Converts the parameters parsed from
`DATABASE_URL` to the concrete configuration types required by persistent.

Currently, only [persistent-postgresql][]'s `PostgresConf` is provided.

[heroku]: http://hackage.haskell.org/package/heroku
[persistent-postgresql]: http://hackage.haskell.org/package/persistent-postgresql

## Installation

```
cabal install heroku-persistent
```

## Example Usage

Adjusting the [Yesod][] scaffold to run on Heroku:

**config/settings.yml**

``` yaml
database-url: "_env:DATABASE_URL:postgres://user:pass@localhost:5432/dbname"
database-pool-size: "_env:DB_POOL:5"
```

**Settings.hs**:

```haskell
import Web.Heroku.Persist.Postgresql (fromDatabaseUrl)

instance FromJSON AppSettings where
    parseJSON = withObject "AppSettings" $ \o -> do
        appDatabaseConf <- fromDatabaseUrl
                             <$> o .: "database-pool-size"
                             <*> o .: "database-url"
        -- ...

        return AppSettings {..}
```

[yesod]: http://www.yesodweb.com

## How to run tests

```
stack test
```
