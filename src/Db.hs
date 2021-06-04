{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Db
  ( eval
  , load
  , run
  ) where

import           Control.Concurrent.MVar (takeMVar, readMVar, putMVar, newMVar, tryPutMVar)
import           Control.Exception (bracket, bracketOnError)
import           Control.Monad (when)
import           Control.Monad.Trans.Except (runExceptT, except)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import           Data.List (foldl')
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           System.IO (withFile, IOMode(..))
import           System.Posix.IO.ByteString (handleToFd, closeFd)

import Types
import Resp
import Fsync

-- Evaluate a command to update the in-memory store.
eval :: Store -> Command -> (Resp, Store)
eval store (SAdd ident args) =
  let set = Set.fromList args
      newStore = Map.insertWith Set.union ident set store
  in (RInt 1, newStore) -- TODO: Return 0 if key is already in set
eval store (SRem ident args) =
  let set = Set.fromList args
      newStore = Map.adjust (flip Set.difference set) ident store
  in (RInt 1, newStore) -- TODO: Return 0 if key was not in set
eval store (SIsmember identKey identSet) =
  let v = case Set.member identSet <$> Map.lookup identKey store of
        Just True -> (RInt 1)
        _ -> (RInt 0)
  in (v, store)
eval store CCommand = (RBulk "OK", store)

-- is a command read-only?
isReadCommand :: Command -> Bool
isReadCommand (SIsmember _ _) = True
isReadCommand CCommand        = True
isReadCommand _               = False

-- Load an AOF and return a new database, or error.
load :: FilePath -> AofMode -> IO (Either String Db)
load path aofMode = do
  contents <- BS.readFile path
  runExceptT $ do
    resps <- except $ decodeManyResp contents
    exprs <- except $ sequence $ map respToCommand resps
    let store = evalCommands exprs
    storeMVar <- liftIO $ newMVar store
    pure $ Db { getStore = storeMVar, getFilePath = path, getFileMode = aofMode }
  where
    evalCommands :: [Command] -> Store
    evalCommands = foldl' (\s e -> snd $ eval s e) Map.empty

-- Run a command in a database, returning the RESP response and the
-- updated database. This will write to the AOF.
run :: Command -> Db -> IO Resp
run expr db

  -- the command is read-only (e.g. SISMEMBER): we just need to "peek"
  -- at the current store.
  | isReadCommand expr = do
      store <- readMVar (getStore db)
      let (res, _) = eval store expr
      pure res

  -- the command updates the store: we acquire a lock on the database,
  -- evaluate the command, write it out to the AOF, update the store
  -- and release the lock.
  | otherwise = do
      let acquireStore = takeMVar (getStore db)
      let ensureLockReleased = \store -> tryPutMVar (getStore db) store

      -- use `bracketOnError` to ensure that if an exception is raised
      -- within this function, we put the old store back in the MVar,
      -- avoiding deadlocking other threads.
      bracketOnError acquireStore ensureLockReleased $ \store -> do
        let (res, s') = eval store expr
        withFile (getFilePath db) AppendMode $ \h -> do
          BS.hPut h $ encodeResp (commandToResp expr)
          when (getFileMode db == Sync) $ do
            -- Convert the Haskell IO handle to a POSIX file descriptor,
            -- so we can call `fsync`. Calling `handleToFd` means that
            -- `withFile` will no longer manage the resource and clean
            -- up: we need to ensure the file descriptor is closed.
            bracket (handleToFd h) closeFd fsync
        putMVar (getStore db) s' -- release lock, update state
        pure res
