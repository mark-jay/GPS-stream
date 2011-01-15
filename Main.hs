module Main where

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

import qualified System.ZMQ as ZMQ

{- map (module name that passed as argument) -> 
       (function that implements main module's functionallity) -}
modules :: Map.Map [Char] ([String] -> ZMQ.Context -> IO (), String)
modules =  Map.fromList [("tracker",	(Tracker.main,		Doc.trackerUsage)),
                         ("parser",	(Parser.main,		Doc.parserUsage)),
                         ("debug",	(ZMQMan.main,		Doc.debugUsage)),
                         ("dumper",	(Dumper.main,		Doc.dumperUsage)),
                         ("sqlSrv",	(Queries.main,		Doc.sqlSrvUsage)),
                         ("frontEnd",	(FrontEndApp.main,	Doc.frontEndUsage))
                        ]

{- entry point -}
main :: IO ()
main = do args <- getArgs
          when (length args == 0) $ 
            Utils.exitWithErr $ Doc.programUsage ++ "(or try --help)"
          dispatch (head args) (tail args)

dispatch :: String -> [String] -> IO ()
dispatch modu1e args | Map.member modu1e modules = dispatchSucc
                     | otherwise		 = dispatchFail
    where dispatchFail = do when ("--help" `elem` (modu1e:args)) $
                              Utils.docAndExit $ Doc.commonHelpMsg $ Map.keys modules
                            Utils.exitWithErr $ Doc.unrecCmd modu1e
          dispatchSucc = do 
            context <- ZMQ.init 1
            let (fn, helpString) = (modules ! modu1e)
            Doc.helpInArgsCheck args helpString
            fn args context
            ZMQ.term context