module Modules.Tracker where

import RMC.API(calcRMCSum)
import qualified Doc
import qualified Utils
import qualified Conf

import System.Random	
import Control.Monad
import Text.Printf(printf)
import Data.Function(on)
import Data.List(intercalate)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC8
import Numeric 
import Data.Char

import System.ZMQ as ZMQ

------------------------------------------------------------------------------
------------------------------ data generation  ------------------------------
------------------------------------------------------------------------------

rand :: IO Int
rand = randR (0, maxBound)

randR :: (Int, Int) -> IO Int
randR (from, to) = getStdRandom (randomR (from, to))

randDR :: (Double, Double) -> IO Double
randDR (from, to) = getStdRandom (randomR (from, to))

randFromList :: [a] -> IO a
randFromList list@(_:_) = do r <- rand 
                             let id = (rem r $ length list)
                             return $ list !! id
randFromList _		= error "randFromList:Empty list"

----------------------------------------
genFixedInt :: (Int, Int) -> IO String
genFixedInt (from, to) = liftM f $ randR (from, to)
    where f :: Int -> String
          f x = zeros x ++ show x
          zeros x = take (on (-) (length . show) to x)
                         (repeat '0')

genFixedDouble :: (Double, Double) -> IO String
genFixedDouble (from, to) = liftM f $ randDR (from, to)
    where f :: Double -> String
          f x = (zeros (truncate x) $ truncate to) ++ showNormaliziedDouble x
          zeros v max = take (on  (-) (length . show) max v) $
                             repeat '0'

-- FIXME fixed size?
showNormaliziedDouble = take 10 . show

ioConcat :: (Monad m) => [m [a]] -> m [a]
ioConcat = liftM concat . sequence

----------------------------------------
-- FIXME make it faster
-- now it takes ~= 0.0004998652s
genRMC :: IO BS.ByteString
genRMC = do time 	<- ioConcat [genFixedInt    (0, 23), 
                                     genFixedInt    (0, 59),
                                     genFixedDouble (0, 59)]
            status 	<- randFromList ["A","V",""]
            lati 	<- ioConcat [genFixedInt 	(0, 89),
                                     genFixedDouble	(0, 59)]
            p 		<- randFromList ["N","S",""]
            longi 	<- ioConcat [genFixedInt 	(0, 179),
                                     genFixedDouble	(0, 59)]
            j 		<- randFromList ["E","W",""]
            v		<- ioConcat [genFixedDouble (0, 10**4)]
            b		<- ioConcat [genFixedDouble (0, 359)]
            date	<- dateGen
            x		<- ioConcat [genFixedDouble (0, 359)]
            n		<- randFromList ["E","W",""]
            m		<- randFromList ["A","D","E","N",""]
            return $ f $ intercalate "," 
                         ["$GPRMC",time,status,lati,p,
                          longi,j,v,b,date,x,n,"*"]
                where f :: String -> BS.ByteString
                      f x = BSC8.pack $ x ++ chkSm x ++ "\13\10" -- FIXME or
                      chkSm x = fixSize 2 $ toHex (calcRMCSum $ BSC8.pack x)

toHex x = showIntAtBase 16 intToDigit x ""

fixSize n string = (take (n - length string) $ repeat '0') ++ string

-- FIXME ?
dateGen = ioConcat [genFixedInt 	(1, 28),	-- day
                    genFixedInt 	(1, 12),	-- month
                    genFixedInt 	(0, 99)]	-- year
------------------------------------------------------------------------------
------------------------------    main logic    ------------------------------
------------------------------------------------------------------------------

main :: [String] -> Context -> IO ()
main args context = do 
  Doc.assertArgs [Doc.HPositiveInt] args

  let addrN = args !! 0

  addr <- liftM Conf.nodeInput $ Conf.getNParser (read addrN)

  {- making connection -}
  oSock <- Utils.mkSockToConn context Push [addr]

  {- sending to server -}
  putStrLn $ "sending to parser on " ++ 
             show addr ++ " ..."
  replicateM_ 1000 $ do
     -- FIXME not forever
     rmc <- genRMC
     send oSock rmc []
     -- threadDelay 10000000

  ZMQ.close oSock
  return ()
