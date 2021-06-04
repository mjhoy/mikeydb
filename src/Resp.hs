{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf        #-}

module Resp
  ( commandToResp
  , decodeOneResp
  , decodeManyResp
  , encodeResp
  , respToCommand
  ) where

import           Control.Applicative
import           Control.Monad (replicateM)
import           Data.Attoparsec.ByteString (parseOnly, Parser, string, many')
import           Data.Attoparsec.ByteString.Char8 (signed, decimal, anyChar, take)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.ByteString.UTF8 (fromString, toString)
import qualified Data.ByteString.Char8 as C
import           Data.Char (toLower)
import           Data.List (foldl')

import Prelude hiding (take)

import Types

respToCommand :: Resp -> Either String Command
respToCommand = go
  where
    -- Non-command types
    go (RInt _)  = Left "integer type is not a command"
    go (RBulk _) = Left "bulk type is not a command"
    go (RErr _)  = Left "error type is not a command"

    -- 0-ary cmds
    go (RArray n ((RBulk cmd):[])) =
      if | C.map toLower cmd == "command" -> Right CCommand
         | otherwise -> Left $ "unrecognized command " <> toString cmd

    -- >= 2-ary cmds
    go (RArray n ((RBulk cmd):(RBulk x):by@(RBulk y):xs)) =
      if | C.map toLower cmd == "sismember" -> Right $ SIsmember x y
         | C.map toLower cmd == "sadd"      -> Right $ SAdd x (map respToIdentifier (by:xs))
         | C.map toLower cmd == "srem"      -> Right $ SRem x (map respToIdentifier (by:xs))
         | otherwise          -> Left $ "unrecognized command " <> toString cmd
      where
        respToIdentifier (RBulk bs) = bs
        respToIdentifier _          = ""

    go (RArray n _) = Left $ "unsupported arguments"

commandToResp :: Command -> Resp
commandToResp = go
  where
    go CCommand = RArray 1 [RBulk "COMMAND"]
    go (SAdd x ys) = RArray (2 + length ys) (RBulk "sadd" : RBulk x : map RBulk ys)
    go (SRem x ys) = RArray (2 + length ys) (RBulk "srem" : RBulk x : map RBulk ys)
    go (SIsmember x y) = RArray 3 (RBulk "sismember" : RBulk x : RBulk y : [])

decodeOneResp :: ByteString -> Either String Resp
decodeOneResp = parseOnly resp

decodeManyResp :: ByteString -> Either String [Resp]
decodeManyResp = parseOnly $ many' resp

encodeResp :: Resp -> ByteString
encodeResp = go
  where
    go (RInt n) = ":" <> fromString (show n) <> crlf'
    go (RErr e) = "-" <> e <> crlf'
    go (RBulk x) =
      "$"
      <> (fromString $ show $ BS.length x)
      <> crlf'
      <> x
      <> crlf'
    go (RArray n xs) =
      "*"
      <> fromString (show n)
      <> crlf'
      <> foldl' (<>) mempty (map encodeResp xs)

resp :: Parser Resp
resp = do
  t <- anyChar
  case t of
    ':' -> RInt <$> signed decimal <* crlf
    '$' -> parserBulk
    '*' -> parserArray
    '-' -> fail $ "error type parsing not implemented!"
    _   -> fail $ "invalid type tag: " <> show t
  where
    parserBulk = do
      n <- signed decimal <* crlf -- should fail on negative/0 lengths?
      RBulk <$> take n <* crlf
    parserArray = do
      n <- signed decimal <* crlf
      RArray n <$> replicateM n resp

crlf :: Parser ()
crlf = string crlf' >> pure ()

crlf' :: ByteString
crlf' = "\r\n"
