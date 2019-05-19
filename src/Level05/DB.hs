{-# LANGUAGE OverloadedStrings #-}
module Level05.DB
  ( FirstAppDB (FirstAppDB)
  , initDB
  , closeDB
  , addCommentToTopic
  , getComments
  , getTopics
  , deleteTopic
  ) where

import           Control.Monad.IO.Class             (liftIO)

import           Data.Text                          (Text)
import qualified Data.Text                          as Text

import           Data.Bifunctor                     (first)
import           Data.Time                          (getCurrentTime)

import           Database.SQLite.Simple             (Connection,
                                                     Query (fromQuery))
import qualified Database.SQLite.Simple             as Sql

import qualified Database.SQLite.SimpleErrors       as Sql
import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)

import           Level05.Types                      (Comment, CommentText,
                                                     Error (DBError), Topic,
                                                     fromDBComment, fromDBTopic,
                                                     getCommentText, getTopic,
                                                     mkTopic)

import           Level05.DB.Types
import           Level05.AppM                       (AppM(..), liftEither)

-- We have a data type to simplify passing around the information we need to run
-- our database queries. This also allows things to change over time without
-- having to rewrite all of the functions that need to interact with DB related
-- things in different ways.
newtype FirstAppDB = FirstAppDB
  { dbConn  :: Connection
  }

runDB
  :: (a -> Either Error b)
  -> IO a
  -> AppM b
runDB f io = 
  AppM $ (\res -> (first DBError res) >>= f) <$> Sql.runDBAction io

-- Quick helper to pull the connection and close it down.
closeDB
  :: FirstAppDB
  -> AppM ()
closeDB app =
  let
    io = (Sql.close . dbConn) app
  in
    runDB pure io

initDB
  :: FilePath 
  -> AppM FirstAppDB
initDB fp =
  let
    query = "CREATE TABLE IF NOT EXISTS comments (id INTEGER PRIMARY KEY, topic TEXT, comment TEXT, time INTEGER)"
    io = do
      conn <- Sql.open fp
      _    <- Sql.execute_ conn query
      pure $ FirstAppDB conn
  in
    runDB pure io

getComments
  :: FirstAppDB
  -> Topic
  -> AppM [Comment]
getComments app topic = 
  let
    conn       = dbConn app
    query      = "SELECT id, topic, comment, time FROM comments WHERE topic = ?"
    retrieval  = Sql.query conn query [getTopic topic]
  in do
    runDB (\cs -> sequence $ fromDBComment <$> cs) retrieval
  
addCommentToTopic
  :: FirstAppDB
  -> Topic
  -> CommentText
  -> AppM ()
addCommentToTopic app topic text =
  let
    conn   = dbConn app 
    query  = "INSERT INTO comments (topic, comment, time) VALUES (?, ?, ?)"
    topic' = getTopic topic
    text'  = getCommentText text

    insertion = getCurrentTime >>= (\t -> Sql.execute conn query (topic', text', t))
  in do
    runDB pure insertion

getTopics
  :: FirstAppDB
  -> AppM [Topic]
getTopics app =
  let
    conn      = dbConn app
    query     = "SELECT DISTINCT topic FROM comments"
    retrieval = Sql.query_ conn query
  in do
    runDB (\ts -> sequence $ fromDBTopic <$> ts) retrieval

deleteTopic
  :: FirstAppDB
  -> Topic
  -> AppM ()
deleteTopic app topic = 
  let
    conn    = dbConn app
    topic'  = getTopic topic
    query   = "DELETE FROM comments WHERE topic = ?"
    removal = Sql.execute conn query [topic']
  in do
    runDB pure removal
-- Go to 'src/Level05/Core.hs' next.
