{-# LANGUAGE OverloadedStrings #-}
module FirstApp.DB
  ( Table (..)
  , FirstAppDB (FirstAppDB)
  , initDB
  , closeDB
  , addCommentToTopic
  , getComments
  , getTopics
  , deleteTopic
  ) where

import           Control.Monad.IO.Class             (liftIO)
import           Control.Monad.Reader               (asks)

import           Data.Bifunctor                     (first)

import qualified Data.Text                          as Text

import           Data.Time                          (getCurrentTime)

import           Database.SQLite.Simple             (Connection, FromRow,
                                                     Query (fromQuery), ToRow)
import qualified Database.SQLite.Simple             as Sql

import qualified Database.SQLite.SimpleErrors       as Sql
import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)

import           FirstApp.DB.Types                  (FirstAppDB (FirstAppDB, dbConn),
                                                     Table (Table, getTableName))
import           FirstApp.Error                     (Error (DBError))
import           FirstApp.Types                     (Comment, CommentText,
                                                     DBFilePath (getDBFilePath),
                                                     Topic, fromDbComment,
                                                     getCommentText, getTopic,
                                                     mkTopic)

import           FirstApp.AppM                      (AppM, envDB, liftEither)

-- Quick helper to pull the connection and close it down.
closeDB
  :: FirstAppDB
  -> IO ()
closeDB =
  Sql.close . dbConn

initDB
  :: DBFilePath
  -> IO ( Either SQLiteResponse FirstAppDB )
initDB fp = Sql.runDBAction $ do
  -- Initialise the connection to the DB...
  -- - What could go wrong here?
  -- - What haven't we be told in the types?
  con <- Sql.open ( getDBFilePath fp )
  -- Initialise our one table, if it's not there already
  _ <- Sql.execute_ con createTableQ
  pure $ FirstAppDB con
  where
  -- Query has an `IsString` instance so string literals like this can be
  -- converted into a `Query` type when the `OverloadedStrings` language
  -- extension is enabled.
    createTableQ = "CREATE TABLE IF NOT EXISTS comments (id INTEGER PRIMARY KEY, topic TEXT, comment TEXT, time INTEGER)"

getDBConn
  :: AppM Connection
getDBConn =
  asks (dbConn . envDB)

runDB
  :: (a -> Either Error b)
  -> (Connection -> IO a)
  -> AppM b
runDB f fa = do
  conn <- getDBConn
  result <- liftIO $ Sql.runDBAction $ fa conn
  let mappedError = first DBError result
  liftEither $ mappedError >>= f

getComments
  :: Topic
  -> AppM [Comment]
getComments topic = runDB (traverse fromDbComment) dbCommand
  where
    sql =  "SELECT id,topic,comment,time FROM comments WHERE topic = ?"
    dbCommand conn = Sql.query conn sql (Sql.Only (getTopic topic))

addCommentToTopic
  :: Topic
  -> CommentText
  -> AppM ()
addCommentToTopic  topic commentText = runDB Right dbCommand
  where
    sql = "INSERT INTO comments (topic,comment,time) VALUES (?,?,?)"
    dbCommand conn = do
      dateTime <- getCurrentTime
      Sql.execute conn sql (getTopic topic, getCommentText commentText, dateTime)

getTopics
  :: AppM [Topic]
getTopics = runDB (traverse (mkTopic . Sql.fromOnly)) dbCommand
  where
    sql = "SELECT DISTINCT topic FROM comments"
    dbCommand conn = Sql.query_ conn sql

deleteTopic
  :: Topic
  -> AppM ()
deleteTopic topic = runDB Right dbCommand
  where
    sql = "DELETE FROM comments WHERE topic = ?"
    dbCommand conn = Sql.execute conn sql (Sql.Only $ getTopic topic)
