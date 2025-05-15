{-# LANGUAGE OverloadedStrings #-}
module Test where

import Control.Applicative
import Options.Applicative
import Options.Applicative.Common
import Options.Applicative.Help.Chunk


data This = One | Two | Three

data Opts
  = PrintInfo
  { somePath :: FilePath
  , someSwitch :: Bool
  }
  | Modify
  { somePath :: FilePath
  , addThis :: This
  }

u = undefined

cli :: Parser Opts
cli
  = asCommands
--  = regular

  where
    asCommands = subparser
       $ command "print-info" (parserInfo printInfo) { infoProgDesc = paragraph "Print the info for path" }
      <> command "modify" (parserInfo modify) { infoProgDesc = paragraph "Modify the path"}

    regular = printInfo <|> modify

    printInfo = PrintInfo
      <$> strOption (path <> help "Path for print-info")
      <*> switch (short 'b' <> long "bool" <> help "Some switch for print-info")
    modify = Modify
      <$> strOption (path <> help "Path for modify")
      <*> option thatParser (short 'a' <> long "add" <> help "Add this for modify")

    path = short 'p' <> long "path"
    thatParser = eitherReader $ \x -> case x of
      "one" -> Right One
      "two" -> Right Two
      "three" -> Right Three
      other -> Left $ "Can't parse " <> show other <> " as This"

parserInfo :: Parser a -> ParserInfo a
parserInfo p = defaultInfo (helper <*> p)

main :: IO ()
main = do
  -- x <- customExecParser (prefs id) (parserInfo cli mempty)
  let p = parserInfo cli
  print $ infoFullDesc p
  x <- execParser p { infoFullDesc = True }

  -- let _ = mapParser undefined cli :: _
  print ()
