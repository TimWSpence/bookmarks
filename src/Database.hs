{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
module Database (
  DbConfig(..),
  Bookmark(..),
  DbError,
  insert,
  list
                ) where

import           Control.Exception
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Yaml
import           GHC.Generics
import           Type.Reflection

data DbConfig = DbConfig {
  dbPath :: String
                         } deriving (Show)

data DbError = DbError String deriving (Show, Typeable)

instance Exception DbError

data Bookmark = Bookmark {
  name :: String,
  url  :: String,
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
  result <- either (\e -> throwError . DbError . prettyPrintParseException $ e) (return . bookmarks) parse
  return result
