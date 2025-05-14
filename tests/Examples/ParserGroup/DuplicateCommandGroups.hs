{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Examples.ParserGroup.DuplicateCommandGroups (opts, main) where

import Data.Semigroup ((<>))
import Options.Applicative

-- This test demonstrates that duplicate + consecutive groups are merged,
-- while duplicate + non-consecutive groups are not merged.

data Command
  = Delete
  | Insert
  | List
  | Print
  | Query
  deriving (Show)

data Sample = Sample
  { hello :: String,
    quiet :: Bool,
    verbosity :: Int,
    cmd :: Command
  }
  deriving (Show)

sample :: Parser Sample
sample =
  Sample
    <$> parseHello
    <*> parseQuiet
    <*> parseVerbosity
    <*> parseCommand

  where
    parseHello =
      strOption
        ( long "hello"
            <> metavar "TARGET"
            <> help "Target for the greeting"
        )

    parseQuiet =
      switch
        ( long "quiet"
            <> short 'q'
            <> help "Whether to be quiet"
        )

    parseVerbosity =
      option
        auto
        ( long "verbosity"
            <> short 'v'
            <> help "Console verbosity"
        )

    parseCommand =
      hsubparser
        ( command "list" (defaultInfo (pure List)) { infoProgDesc = "Lists elements"}
            <> commandGroup "Info commands"
        )
        <|> hsubparser
          ( command "delete" (defaultInfo (pure Delete)) { infoProgDesc = "Deletes elements"}
              <> commandGroup "Update commands"
          )
        <|> hsubparser
          ( command "insert" (defaultInfo (pure Insert)) { infoProgDesc = "Inserts elements"}
              <> commandGroup "Update commands"
          )
        <|> hsubparser
          ( command "query" (defaultInfo (pure Query)) { infoProgDesc = "Runs a query"}
          )
        <|> hsubparser
        ( command "print" (defaultInfo (pure Print)) { infoProgDesc = "Prints table"}
            <> commandGroup "Info commands"
        )

opts :: ParserInfo Sample
opts =
  (defaultInfo
    (sample <**> helper))
    { infoProgDesc = "Duplicate consecutive command groups consolidated"
    , infoHeader = "parser_group.duplicate_command_groups - a test for optparse-applicative"
    }

main :: IO ()
main = do
  r <- customExecParser defaultPrefs { prefHelpShowGlobal = True } opts
  print r
