module Dumper where

import System.ZMQ as ZMQ
import Control.Monad
import Doc

main :: [String] -> IO ()
main args = do Doc.lengthArgsCheck args 0
               Doc.helpInArgsCheck args helpAboutModule
               
               let connectTo = args !! 0
               
               context <- ZMQ.init 1 	-- size
               sock <- socket context Pull
               connect sock connectTo

               -- FIXME not forever
               forever $ do
                 rmc <- receive sock []
                 return ()
                 
                 
                 -- BSC8.appendFile "test.test" rmc
                 
               ZMQ.close sock
               ZMQ.term context
                 -- threadDelay 10000000
               return ()

helpAboutModule = ""