{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeApplications           #-}
module Main where

import           Cli
import           Control.Exception
import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.ByteString.Char8 as B
import           Data.Coerce
import           Data.Foldable
import           Data.Yaml
import           Database
import           Options.Applicative
import           System.Environment
import           System.Process

main :: IO ()
main = do
  cmd <- execParser commandParser
  print cmd
  r   <- runExceptT . flip runReaderT (DbConfig "/Users/tim/dev/bucket/bookmarks/bookmarks.yml") . runApp $ handleCommand cmd
  _   <- either (\e -> throwIO e) (return) r
  return ()

newtype App a = App { runApp :: ReaderT DbConfig (ExceptT DbError IO) a } deriving (Functor, Applicative, Monad, MonadReader DbConfig, MonadError DbError, MonadIO)

handleCommand :: Command -> App ()
handleCommand = \case
  List -> do
    bookmarks <- list
    display bookmarks
  Add AddOptions{..} -> insert name url tags
  Search SearchOptions{..} -> do
    bookmarks <- search _pattern
    display bookmarks
  Edit -> do
    path <- reader dbPath
    editor <- fmap (maybe (error "$EDITOR is not set") id) . liftIO $ lookupEnv "EDITOR"
    liftIO $ createProcess (shell $ editor <> " " <> path)
    return ()
  where
    display :: [Bookmark] -> App ()
    display bms = liftIO $ traverse_ (B.putStrLn . encode) bms
