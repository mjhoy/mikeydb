{-# LANGUAGE BangPatterns #-}

module Types where

import Control.Concurrent.MVar (MVar)
import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Set (Set)
import Data.Int (Int64)

type Identifier = ByteString
type Store = Map Identifier (Set Identifier)

data AofMode = Sync | NoSync
  deriving (Eq, Show)

data Command
  = SAdd !Identifier ![Identifier]
  | SRem !Identifier ![Identifier]
  | SIsmember !Identifier !Identifier
  | CCommand
  deriving (Eq, Show)

data Db = Db
  { getStore :: MVar Store
  , getFilePath :: !FilePath
  , getFileMode :: !AofMode
  }

data Resp
  = RBulk !ByteString
  | RInt !Int64
  | RErr !ByteString
  | RArray !Int ![Resp]
  deriving (Eq, Ord, Show)

data Program = Program
  { getAofPath :: !String
  , getPort :: !FilePath
  , getSync :: !Bool
  } deriving (Eq, Show)
