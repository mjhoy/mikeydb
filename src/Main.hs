{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Monad (unless)
import qualified Data.ByteString as BS
import           Data.ByteString.UTF8 (fromString)
import           Network.Socket (Socket)
import           Network.Socket.ByteString (recv, sendAll)
import           Options.Applicative
import           System.Exit (exitFailure)
-- import           Debug.Trace (trace, traceM)

import           Types
import           Resp
import qualified Db as Db
import qualified Server as Server

main :: IO ()
main = do
  p <- parseProgramArgs

  let path = getAofPath p
  let sync = getSync p
  db <- load' path sync

  Server.start (getPort p) (handler db)

  where

    load' :: FilePath -> Bool -> IO Db
    load' path sync = do
      let syncMode = if sync then Sync else NoSync
      initDb <- Db.load path syncMode
      case initDb of
        Left err -> do
          putStrLn $ "Error loading file: " <> err
          exitFailure
        Right db -> pure db

    handler :: Db -> Socket -> IO ()
    handler db conn = do
      msg <- recv conn 1024
      unless (BS.null msg) $ do
        case decodeOneResp msg >>= respToCommand of
          Right expr -> do
            res <- Db.run expr db
            sendAll conn $ encodeResp res
          Left err -> do
            putStrLn $ "Error parsing: " <> err
            putStrLn $ show $ decodeOneResp msg
            sendAll conn $ encodeResp (RErr (fromString err))
        handler db conn


parseProgramArgs :: IO Program
parseProgramArgs = execParser opts
  where
    opts = info (program <**> helper)
      ( fullDesc
     <> progDesc "Start server listening on PORT"
     <> header "mikeydb -- simple database" )

program :: Parser Program
program = Program
       <$> strOption
           ( long "file"
          <> short 'f'
          <> metavar "FILE"
          <> help "AOF database file"
          <> value "mikeydb.aof" )
       <*> strOption
           ( long "port"
          <> short 'p'
          <> metavar "PORT"
          <> help "Port to listen for connections"
          <> value "6333" )
       <*> switch
           ( long "sync"
          <> short 's'
          <> help "Sync disk operations" )
