module Doc where

import Control.Monad
import qualified Data.Maybe as Maybe
import qualified Data.List  as List
import qualified Utils
import qualified Conf

{- this module consist error messages, 
   assertions, documentation etc -}

---------------------------------------------------------------------------------------
-- asserts and checks

lengthArgsAssert cond = do when (not cond) $
                             Utils.exitWithErr wrongNumArgs

helpInArgsCheck args helpString = do when ("--help" `elem` args) $
                                       Utils.docAndExit helpString

{-
addressExistAssert string n = do addr <- Conf.getMaybeAddr string n
                                 when (Maybe.isNothing addr) $
                                   exitWithErr $ "address " ++ string ++ ":" ++ show n ++ "does not exist"
-}

naturalNumAssert string err = if (reads :: ReadS Int) string /= []
                              then return ()
                              else Utils.exitWithErr err

-- FIXME
correctFileNameAssert :: String -> IO ()         
correctFileNameAssert fileName = return ()
---------------------------------------------------------------------------------------
-- labels 

wrongNumArgs = "invalid number of arguments(try --help)"

programUsage = "usage: gps-stream <module> <module-args>"

unrecCmd cmd = "unrecognised command:" ++ cmd ++ "(try --help)"

commonHelpMsg modules = Doc.programUsage ++ "\nlist of known module are:\n" ++ 
                        List.intercalate "\n" modules ++ "\n\n" ++
                        "You should lauch modules in the following order:\n" ++ 
                        "    parser, dumper, sqlSrv, frontEnd, tracker" ++ "\n\n" ++ 
                        "For more information about a module use:\n    gps-stream <module> --help"

trackerUsage = "usage: gps-stream tracker <number>\n  where <number> is number of " ++ 
               "parser you want connect this tracker to. For example:\n gps-stream tracker 0"

parserUsage = "usage: gps-stream parser <srv-addr-id>\nFor example:" ++ 
              "\n  gps-stream parser 0"

dumperUsage = "usage: gps-stream dumper <fileToWrite>\n" ++ "You may use 'dmp.txt'\n" ++ 
              "this module connects to every parser listed in conf.cfg"

sqlSrvUsage = "usage: gps-stream sqlSrv <queryIdx> <number>\n  where <number> is number of " ++ 
              "agregator you want connect this server to.\n For example:\n gps-stream sqlSrv 0 0"

frontEndUsage = "usage: gps-stream frontEnd\n" ++
                "this module connects to every agregator listed in conf.cfg"
