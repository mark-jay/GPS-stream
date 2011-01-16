module Modules.Debug where

import System.ZMQ as ZMQ
import Utils
import Control.Monad

{- Only aim of this module is debug and trace -}

-- sender
main _ context = do 
  sock <- socket context Pull

  addr <-  askFor "bind to what?"
  bind sock addr
  -- addr1 <- askFor "bind to what(2)?"
  -- bind sock addr1
            
  askFor "enter to receive"
  forever $ do
             msg <- receive sock []
             print msg
             return ()
          
