module Main where

import Lib
import Options.Applicative
import Cli

main :: IO ()
main = do
  cmd <- execParser commandParser
  print cmd
