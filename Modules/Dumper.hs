module Modules.Dumper(main) where

import System.ZMQ as ZMQ
-- import 			Text.ProtocolBuffers.Basic		-- for uFromString
import qualified	Text.ProtocolBuffers.WireMessage as Protobuf

import Control.Monad
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.List as List

import Doc
import Utils
import Conf

main :: [String] -> Context -> IO ()
main args context = do 
  Doc.assertArgs [HFilePath] args

  let fileToDump   = args !! 0

  connectTo <- liftM (map nodeOutput) $ Conf.getParsers

  sock <- mkSockToConn context Sub connectTo
  subscribe sock ""

  -- FIXME not forever
  putStrLn $ "dumping from {" ++ List.intercalate ", " connectTo ++ 
             "} on " ++ show fileToDump
  forever $ do
    rmc <- receive sock []
    -- FIXME not append, should use open/closeFile
    BSC8.appendFile fileToDump rmc
    return ()

  ZMQ.close sock
  return ()
