{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE TypeApplications           #-}
module Main where

import           Cli
import           Control.Exception
import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.ByteString.Char8 as B
import           Data.Foldable
import           Data.Yaml
import           Database
import           Lib
import           Options.Applicative

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
    liftIO $ putStrLn "handling list"
    bookmarks <- list
    liftIO $ traverse_ (B.putStrLn . encode) bookmarks
  _    -> error "Not implemented"
