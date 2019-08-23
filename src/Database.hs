{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}

module Database (
  DbConfig(..),
  Bookmark(..),
  DbError,
  Pattern(..),
  insert,
  list,
  search
                ) where

import           Control.Exception
import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.List            as DL
import           Data.Yaml
import           GHC.Generics
import           Text.Regex.PCRE
import           Type.Reflection

data DbConfig = DbConfig {
  dbPath :: String
                         } deriving (Show)

data DbError = DbError String deriving (Show, Typeable)

instance Exception DbError

-- Ideally these fields would be newtypes but I couldn't be
-- bothered to make {To,From}JSON instances which unwrapped
-- the newtype instead of treating it as a record
data Bookmark = Bookmark {
  name :: String,
  url  :: String,
  tags :: [String]
                         } deriving (Show, Generic)

newtype Bookmarks = Bookmarks {
  bookmarks :: [Bookmark]
                           } deriving (Show, Generic)

instance FromJSON Bookmark where
instance ToJSON Bookmark where

instance FromJSON Bookmarks where
instance ToJSON Bookmarks where

data Pattern = Literal String
             | Regex String
             deriving (Show)

list :: (MonadReader DbConfig m, MonadError DbError m, MonadIO m) => m [Bookmark]
list = do
  path   <- reader dbPath
  parse  <- liftIO $ decodeFileEither @Bookmarks path
  either (\e -> throwError . DbError . prettyPrintParseException $ e) (return . bookmarks) parse

insert :: (MonadReader DbConfig m, MonadError DbError m, MonadIO m) => String -> String -> [String] -> m ()
insert n u t = do
  path   <- reader dbPath
  current <- list
  let new = Bookmark{name = n, url = u, tags = t}
  let updated = new : current
  liftIO . encodeFile path . Bookmarks $ updated

search :: (MonadReader DbConfig m, MonadError DbError m, MonadIO m) => Pattern -> m [Bookmark]
search (Literal p) = do
  current <- list
  return . filter (\Bookmark{..} -> matches name || matches url || any matches tags) $ current
  where
    matches = DL.isInfixOf p
search (Regex p) = do
  current <- list
  return . filter (\Bookmark{..} -> matches name || matches url || any matches tags) $ current
  where
    matches t = t =~ p :: Bool
