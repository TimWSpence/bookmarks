{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cli (
  Command(..),
  AddOptions(..),
  SearchOptions(..),
  commandParser
           ) where

import Options.Applicative
import Data.Text
import Database(Pattern(..))
import Text.Regex.PCRE

data Command
  = Add AddOptions
  | Edit
  | List
  | Search SearchOptions
  deriving (Show)

data SearchOptions = SearchOptions {
  _pattern :: Pattern
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
    searchOptions = SearchOptions <$> _pattern

    _pattern :: Parser Pattern
    _pattern = parse <$> strArgument (help "Pattern to search for. May be a literal or a /regex/" <> metavar "PATTERN")
               where
                 parse :: String -> Pattern
                 parse str = match
                               where
                                 pat :: String = "/(.*)/"
                                 (_,_,_,m) = str =~ pat :: (String, String, String, [String]) -- I know, right? :(
                                 match = case m of
                                   [] -> Literal str
                                   [p] -> Regex p
                                   _ -> error $ "Failed to parse pattern " <> str

    addOptions :: Parser AddOptions
    addOptions = AddOptions <$> name <*> url <*> tags

    name :: Parser String
    name = strArgument (help "Name of bookmark" <> metavar "NAME")

    url :: Parser String
    url = strArgument (help "URL for bookmark" <> metavar "URL")

    tags :: Parser [String]
    tags = (fmap unpack . splitOn (",") . pack) <$> strArgument (help "Tags for bookmark, comma separated" <> metavar "TAGS")
