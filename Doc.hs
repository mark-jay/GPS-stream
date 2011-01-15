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

data HType = HPositiveInt
           | HFilePath
           | HRangedInt { hMinInt	:: Int
                        , hMaxInt	:: Int }
             deriving ( Show )
hMatch' :: HType -> String -> Bool
hMatch' HPositiveInt 		int	 = (reads :: ReadS Int) int /= []
hMatch' HFilePath 		fPath	 = True
hMatch' (HRangedInt min max) 	int	 = hMatch' HPositiveInt int && inRange (read int)
    where 
      inRange int | int >= min && int <= max 	= True
                  | otherwise			= False

hMatch :: HType -> String -> IO ()
hMatch htype rtype= do
  when (not $ hMatch' htype rtype) $ do
       Utils.exitWithErr $ "Couldn't match expected type `" ++ show htype ++ "'" ++
                           "against input `" ++ rtype ++ "'"

assertArgs :: [HType] -> [String] -> IO ()
assertArgs htypes args = allMatch >> lenMatch
    where 
      allMatch = sequence $ zipWith hMatch htypes args
      lenMatch = lengthArgsAssert $ length args >= length htypes

      lengthArgsAssert cond = do when (not cond) $
                                      Utils.exitWithErr $ wrongNumArgsExp $ length htypes

--------------------------------------------------------------------------------

helpInArgsCheck args helpString = do when ("--help" `elem` args) $
                                       Utils.docAndExit helpString

{-
addressExistAssert string n = do addr <- Conf.getMaybeAddr string n
                                 when (Maybe.isNothing addr) $
                                   exitWithErr $ "address " ++ string ++ ":" ++ show n ++ "does not exist"
-}
---------------------------------------------------------------------------------------
-- labels 

wrongNumArgsExp :: Int -> String
wrongNumArgsExp n = "invalid number of arguments(try --help)\n  Expected at least " ++ show n

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

debugUsage = "just for debug"

dumperUsage = "usage: gps-stream dumper <fileToWrite>\n" ++ "You may use 'dmp.txt'\n" ++ 
              "this module connects to every parser listed in conf.cfg"

sqlSrvUsage = "usage: gps-stream sqlSrv <queryIdx> <number>\n  where <number> is number of " ++ 
              "agregator you want connect this server to.\n For example:\n gps-stream sqlSrv 0 0"

frontEndUsage = "usage: gps-stream frontEnd\n" ++
                "this module connects to every agregator listed in conf.cfg"
