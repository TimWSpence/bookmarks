{-# LANGUAGE OverloadedStrings #-}
module Cli (
  Command(..),
  commandParser
           ) where

import Options.Applicative
import Data.Text

data Command
  = Add AddOptions
  | Edit
  | List
  | Search SearchOptions
  deriving (Show)

data SearchOptions = SearchOptions {
  regex :: String
                                   } deriving (Show)

data AddOptions = AddOptions {
  name :: String,
  url  :: String,
  tags :: [String]
                              } deriving (Show)

commandParser :: ParserInfo Command
commandParser = info (commands <**> helper) (fullDesc <> progDesc "A CLI-based bookmarks manager" <> header "bm")

commands :: Parser Command
commands = subparser (
  (command "search" (info (Search <$> searchOptions) (progDesc "Search for bookmarks")))
  <> (command "edit" (info (pure Edit)(progDesc "Edit the bookmarks file using $EDITOR")))
  <> (command "list" (info (pure List)(progDesc "List all bookmarks")))
  <> (command "add" (info (Add <$> addOptions)(progDesc "Add a bookmark")))
  )
  where
    searchOptions :: Parser SearchOptions
    searchOptions = SearchOptions <$> regex

    regex :: Parser String
    regex = strArgument (help "Pattern to search for" <> metavar "PATTERN")

    addOptions :: Parser AddOptions
    addOptions = AddOptions <$> name <*> url <*> tags

    name :: Parser String
    name = strArgument (help "Name of bookmark" <> metavar "NAME")

    url :: Parser String
    url = strArgument (help "URL for bookmark" <> metavar "URL")

    tags :: Parser [String]
    tags = (fmap unpack . splitOn (",") . pack) <$> strArgument (help "Tags for bookmark, comma separated" <> metavar "TAGS")
