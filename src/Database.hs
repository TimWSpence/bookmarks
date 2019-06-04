{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
module Database (
  DbConfig(..),
  Bookmark(..),
  DbError,
  insert,
  list
                ) where

import Control.Monad.Reader
import Control.Monad.Except
import Data.Yaml
import GHC.Generics

data DbConfig = DbConfig {
  dbPath :: String
                         } deriving (Show)

data DbError = DbError String

data Bookmark = Bookmark {
  name :: String,
  url :: String,
  tags :: [String]
                         } deriving (Show, Generic)

data Bookmarks = Bookmarks {
  bookmarks :: [Bookmark]
                           } deriving (Show, Generic)

instance FromJSON Bookmark where

instance ToJSON Bookmark where

instance FromJSON Bookmarks where

instance ToJSON Bookmarks where

insert :: Bookmark -> m ()
insert = undefined

list :: (MonadReader DbConfig m, MonadError DbError m, MonadIO m) => m [Bookmark]
list = do
  path   <- reader dbPath
  parse  <- liftIO $ decodeFileEither @Bookmarks path
  liftIO $ print parse
  result <- either (\e -> throwError . DbError . prettyPrintParseException $ e) (return . bookmarks) parse
  return result
