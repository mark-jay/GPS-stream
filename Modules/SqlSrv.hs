module Modules.SqlSrv(main) where

import SqlStream.TableMap (TableMap)
import qualified SqlStream.TableMap as TableMap
import SqlStream.Fields

import Doc
import qualified Utils
import qualified Logger
import Conf

import qualified RMC.API 		as RMC
import RMC.API(RMC)
import qualified Text.ProtocolBuffers.WireMessage as Protobuf

import System.ZMQ as ZMQ
import IO

import Control.Monad
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.List as List

import qualified Data.Map {- remove it -}
import qualified Data.Maybe as Maybe

import Prelude hiding((.), id)
import Control.Category
import Control.Monad
import Control.Arrow((&&&))

import qualified RMC.Protobuf.RMC.RMC.Status	as RMCStatus
import qualified RMC.Protobuf.RMC.RMC.ModeInd	as RMCModeInd
import qualified RMC.Protobuf.RMC.RMC.MDeclDir	as RMCMdeclDir

type Row = [Field]

queries :: [TableMap [Field] RMC Row]
queries = [query1, query2]

-- number of message to receive
maxMess = 1000

inQueriesRangeAssert :: Int -> IO ()
inQueriesRangeAssert x = if x < length queries && x >= 0
                         then return ()
                         else Utils.exitWithErr "SqlSrv: no such query"

--------------------------------------------------------------------------------
-- queries

-- average time
query1 :: TableMap [Field] RMC Row
query1 = TableMap.map RMC.time >>> 
         TableMap.filter (/= Nothing) >>> 
         TableMap.map Maybe.fromJust >>>
         TableMap.avrg (const []) >>> 
         TableMap.map (FDouble . Just . snd) >>>
         TableMap.map return 

-- sum speed, group by status
query2 :: TableMap [Field] RMC Row
query2 = TableMap.mkTMapWithKeys' (toKey &&& toValue) (+) 0 >>>
         TableMap.map (TableMap.mapSnd $ return . FDouble . Just) >>>
         TableMap.map (\(a, b) -> a++b)
    where toKey   = return . FRMCStatus . RMC.status
          toValue = Maybe.fromMaybe 0 . RMC.speed


--------------------------------------------------------------------------------

strToTime :: String -> Field
strToTime = const time
    where time = FDayTime $ Just $ read "2010-12-10 19:21:55.810178 UTC"

--------------------------------------------------------------------------------


main :: [String] -> Context -> IO ()
main args context = do
  Doc.assertArgs [HRangedInt 0 (length queries - 1), Doc.HPositiveInt] args

  let queryIdx :: Int
      queryIdx  = read $ args !! 0
      query	= queries !! queryIdx
      agregIdx	= read $ args !! 1

  connectTo 	<- liftM (map nodeOutput) $ Conf.getParsers
  toBindO	<- liftM nodeOutput 	  $ Conf.getNAgreg agregIdx

  (iSock, oSock) <- zmqInit connectTo toBindO context

  putStrLn $ "serving {" ++ List.intercalate ", " connectTo ++ 
        "} with query " ++ show queryIdx

  rows <- liftM concat $ replicateM maxMess $ do
            rmc <- receive iSock []
            case Protobuf.messageGet (Utils.toLazyBS rmc) of
              (Left err)  	    -> do Logger.messageGetError rmc err
                                          return []
              -- do send oSock (Utils.fromLazyBS $ Protobuf.messagePut rmc) []
              (Right (rmc, rest))   -> return [rmc]
  let result	= TableMap.elems $ TableMap.fromList rows >>> query
      msg 	= BSC8.pack $ show $ result

  -- debug info
  putStrLn $ "sending " ++ show result

  send oSock msg []

  return ()
      where
        termAll iSock oSock = do              
          ZMQ.close oSock
          ZMQ.close iSock
          return ()

        zmqInit connectTo toBindO context = do
          {- making input connection -}             
          iSock <- Utils.mkSockToConn context Sub connectTo
          subscribe iSock ""

          {- making output connection -}
          oSock <- Utils.mkSockToBind context Pub [toBindO]
          return (iSock, oSock)