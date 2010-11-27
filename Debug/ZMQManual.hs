module Debug.ZMQManual where

import System.ZMQ as ZMQ
import Utils
import Control.Monad

-- sender
main _ = do addr <- askFor "connect to what?"
            
            context <- ZMQ.init 1 	-- size
            sock <- socket context Pull
            connect sock addr
            
            forever $ do
              askFor "enter to receive"
              msg <- receive sock []
              print msg
              return ()
          
