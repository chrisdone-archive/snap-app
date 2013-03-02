{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Model running.

module Snap.App.Model
  (model
  ,runDB
  ,query
  ,single
  ,singleNoParams
  ,queryNoParams
  ,exec
  ,DB.Only(..))
  where

import           Control.Monad.Env                       (env)
import           Control.Monad.Reader
import           Data.String
import           Database.PostgreSQL.Base   (withPoolConnection,withTransaction)
import           Database.PostgreSQL.Simple              (Only(..))
import qualified Database.PostgreSQL.Simple              as DB
import           Database.PostgreSQL.Simple (Pool)
import           Database.PostgreSQL.Simple.QueryParams
import           Database.PostgreSQL.Simple.QueryResults
import           Snap.App.Types

-- | Run a model action at the top-level.
runDB :: s -> c -> Pool -> Model c s () -> IO ()
runDB st conf pool mdl = do
  withPoolConnection pool $ \conn -> do
    withTransaction conn $ do
      let state = ModelState conn st conf
      -- Default to HTML, can be overridden.
      runReaderT (runModel mdl) state

-- | Run a model action from within a controller.
model :: AppLiftModel c s => Model c s a -> Controller c s a
model = liftModel

-- | Query with some parameters.
query :: (QueryParams ps,QueryResults r) => [String] -> ps -> Model c s [r]
query q ps = do
  conn <- env modelStateConn
  Model $ ReaderT (\_ -> DB.query conn (fromString (unlines q)) ps)

-- | Query a single field from a single result.
single :: (QueryParams ps,QueryResults (Only r)) => [String] -> ps -> Model c s (Maybe r)
single q ps = do
  rows <- query q ps
  case rows of
    [(Only r)] -> return (Just r)
    _          -> return Nothing

-- | Query a single field from a single result (no params).
singleNoParams :: (QueryResults (Only r)) => [String] -> Model c s (Maybe r)
singleNoParams q = do
  rows <- queryNoParams q
  case rows of
    [(Only r)] -> return (Just r)
    _          -> return Nothing

-- | Query with no parameters.
queryNoParams :: (QueryResults r) => [String] -> Model c s [r]
queryNoParams q = do
  conn <- env modelStateConn
  Model $ ReaderT (\_ -> DB.query_ conn (fromString (unlines q)))

-- | Execute some SQL returning the rows affected.
exec :: (QueryParams ps) => [String] -> ps -> Model c s Integer
exec q ps = do
  conn <- env modelStateConn
  Model $ ReaderT (\_ -> DB.execute conn (fromString (unlines q)) ps)
