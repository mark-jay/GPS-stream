module Main where

import qualified RMC.Protobuf.RMC.RMC	 	as RMC
import qualified RMC.Parser.RMC 		as Parser
import qualified RMC.API			as RMC
import qualified Tracker.Tracker		as Tracker
import qualified Doc
import qualified Debug.ZMQManual		as ZMQMan



-- import Text.ProtocolBuffers.WireMessage
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC8
import System.IO
import Control.Monad
import System.Environment
import qualified Data.Map  as Map
import Data.Map((!))
import qualified Data.List as List


modules = Map.fromList [("tracker", Tracker.main),
                        ("parser",  Parser.main),
                        ("debug",  ZMQMan.main)
                       ]

main :: IO ()
main = do args <- getArgs
          when (length args == 0) $ 
            Doc.exitWithErr $ usage ++ "(or try --help)"
          dispatch (head args) (tail args)

dispatch :: String -> [String] -> IO ()
dispatch modu1e args | Map.member modu1e modules = modules ! modu1e $ args
                     | otherwise		 = dispatchFail
    where dispatchFail = do when ("--help" `elem` (modu1e:args)) $
                              Doc.docAndExit commonHelpMsg
                            Doc.exitWithErr $ unrecCmd modu1e

usage = "usage: gps-stream <module> <module-args>"

unrecCmd cmd = "unrecognised command:" ++ cmd ++ "(try --help)"

commonHelpMsg = usage ++ "\nlist of known module are:\n" ++ 
                List.intercalate "\n" (Map.keys modules) ++
                "\n\nFor more information about a module use:\n    gps-stream <module> --help"
