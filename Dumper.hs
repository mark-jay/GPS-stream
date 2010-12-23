module Dumper where

import System.ZMQ as ZMQ
-- import 			Text.ProtocolBuffers.Basic		-- for uFromString
import qualified	Text.ProtocolBuffers.WireMessage as Protobuf

import Control.Monad
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.List as List

import Doc
import Utils
import Conf

main :: [String] -> IO ()
main args = do 
  Doc.lengthArgsAssert (length args > 0)
  Doc.helpInArgsCheck args Doc.dumperUsage
               
  let fileToDump   = args !! 0

  connectTo <- liftM (map nodeOutput) $ Conf.getParsers

  Doc.correctFileNameAssert fileToDump
               
  context <- ZMQ.init 1
  sock <- socket context Sub
  subscribe sock ""

  forM_ connectTo $ \ parserAddr -> do
      connect sock parserAddr

  -- FIXME not forever
  putStrLn $ "dumping from {" ++ List.intercalate ", " connectTo ++ 
             "} on " ++ show fileToDump
  forever $ do
    rmc <- receive sock []
    -- FIXME not append, should use open/closeFile
    BSC8.appendFile fileToDump rmc
    return ()

  ZMQ.close sock
  ZMQ.term context
  return ()
