module Main where

import qualified Data.Attoparsec.ByteString as DAP
import qualified Data.ByteString            as BS
import           Options.Applicative
import           Parse                      (GdsRecordT, parseGdsRecord)

newtype Command = Dump FilePath

commandParser :: Parser Command
commandParser = subparser
  ( command "dump" (info (dumpParser <**> helper) (progDesc "Parse a GDS file and print its records")) )

dumpParser :: Parser Command
dumpParser = Dump <$> argument str (metavar "FILE" <> help "Path to a GDS file")

main :: IO ()
main = do
  cmd <- execParser opts
  case cmd of
    Dump path -> runDump path
  where
    opts = info (commandParser <**> helper)
      ( fullDesc <> progDesc "Gandalf: parse and inspect GDSII files" )

runDump :: FilePath -> IO ()
runDump path = do
  contents <- BS.readFile path
  mapM_ print (parseAllRecords contents)

parseAllRecords :: BS.ByteString -> [GdsRecordT]
parseAllRecords = go
  where
    go bs
      | BS.null bs = []
      | otherwise = case DAP.parse parseGdsRecord bs of
          DAP.Done rest r    -> r : go rest
          DAP.Fail _ _ msg   -> error ("failed to parse GDS record: " ++ msg)
          DAP.Partial cont   -> case cont BS.empty of
            DAP.Done rest r  -> r : go rest
            DAP.Fail _ _ msg -> error ("failed to parse GDS record: " ++ msg)
            DAP.Partial _    -> error "parser requested more input after EOF"
