module Main where

import qualified RMC.Protobuf.RMC.RMC	 	as RMC
import qualified RMC.Parser.RMC 		as Parser
import qualified RMC.API			as RMC
import qualified Tracker.Tracker		as Tracker
import qualified Doc
import qualified Utils
import qualified Dumper
import qualified Debug.ZMQManual		as ZMQMan
import qualified SqlStream.Queries		as Queries
import qualified FrontEndApp

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC8
import System.IO
import Control.Monad
import System.Environment
import qualified Data.Map  as Map
import Data.Map((!))
import qualified Data.List as List


{- map (module name that passed as argument) -> 
       (function that implements main module's functionallity) -}
modules :: Map.Map [Char] ([String] -> IO ())
modules =  Map.fromList [("tracker",	Tracker.main),
                         ("parser",	Parser.main),
                         ("debug",	ZMQMan.main),
                         ("dumper",	Dumper.main),
                         ("sqlSrv",	Queries.main),
                         ("frontEnd",	FrontEndApp.main)
                        ]

{- entry point -}
main :: IO ()
main = do args <- getArgs
          when (length args == 0) $ 
            Utils.exitWithErr $ Doc.programUsage ++ "(or try --help)"
          dispatch (head args) (tail args)

dispatch :: String -> [String] -> IO ()
dispatch modu1e args | Map.member modu1e modules = modules ! modu1e $ args
                     | otherwise		 = dispatchFail
    where dispatchFail = do when ("--help" `elem` (modu1e:args)) $
                              Utils.docAndExit $ Doc.commonHelpMsg $ Map.keys modules
                            Utils.exitWithErr $ Doc.unrecCmd modu1e
