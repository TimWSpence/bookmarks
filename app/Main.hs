{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Lib
import Options.Applicative
import Cli
import Database
import Control.Monad.Except
import Control.Monad.Reader
import Data.Foldable

main :: IO ()
main = do
  cmd <- execParser commandParser
  print cmd
  r   <- runExceptT . flip runReaderT (DbConfig "/Users/tim/dev/bucket/bookmarks.yml") . runApp $ handleCommand cmd
  return ()

newtype App a = App { runApp :: ReaderT DbConfig (ExceptT DbError IO) a } deriving (Functor, Applicative, Monad, MonadReader DbConfig, MonadError DbError, MonadIO)

handleCommand :: Command -> App ()
handleCommand = \case
  List -> do
    liftIO $ putStrLn "handling list"
    bookmarks <- list
    liftIO $ traverse_ print bookmarks
  _    -> error "Not implemented"
