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

**Application.hs**:

```haskell
import Web.Heroku.Persist.Postgresql (postgresConf)

makeFoundation :: AppConfig DefaultEnv Extra -> IO App
makeFoundation conf = do
    -- ...

    dbconf <- postgresConf 10

    p <- Database.Persist.createPoolConfig (dbconf :: Settings.PersistConf)

    -- ...
```

[yesod]: http://www.yesodweb.com

## How to run tests

```
stack test
```
