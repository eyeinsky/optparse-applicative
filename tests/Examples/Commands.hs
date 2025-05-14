{-# LANGUAGE CPP #-}
module Examples.Commands where

import Data.List
import Data.Monoid
import Options.Applicative

#if __GLASGOW_HASKELL__ <= 702
(<>) :: Monoid a => a -> a -> a
(<>) = mappend
#endif

data Sample
  = Hello [String]
  | Goodbye
  deriving (Eq, Show)

hello :: Parser Sample
hello = Hello <$> many (argument str (metavar "TARGET..."))

sample :: Parser Sample
sample = subparser
       ( command "hello"
         (defaultInfo hello)
               { infoProgDesc = "Print greeting" }
      <> command "goodbye"
         (defaultInfo (pure Goodbye))
               { infoProgDesc = "Say goodbye" }
       )
      <|> subparser
       ( command "bonjour"
         (defaultInfo hello)
               { infoProgDesc = "Print greeting" }
      <> command "au-revoir"
         (defaultInfo (pure Goodbye))
               { infoProgDesc = "Say goodbye" }
      <> commandGroup "French commands:"
      <> hidden
       )

run :: Sample -> IO ()
run (Hello targets) = putStrLn $ "Hello, " ++ intercalate ", " targets ++ "!"
run Goodbye = putStrLn "Goodbye."

opts :: ParserInfo Sample
opts = defaultInfo (sample <**> helper)

main :: IO ()
main = execParser opts >>= run
