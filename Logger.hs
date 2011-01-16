module Logger(rmcParseError, messageGetError, 
              resultReadError) 
    where

import RMC.API

import qualified Data.ByteString.Char8 as BSC8

logFile = "log.log"

rmcParseError :: String -> String -> IO ()
rmcParseError onInput err = do
  appendFile logFile $ "\n\nrmcParseError:" ++ onInput ++
                       "  With error:" ++ err

{-
fromProtoCastErr :: RMC -> IO ()
fromProtoCastErr rmc = do
  appendFile logFile $ "\n\nfromProtoCastErr:" ++ show rmc
-}

resultReadError :: BSC8.ByteString -> IO ()
resultReadError input = appendFile logFile $ 
                                   "resultReadError on input:\n" ++
                                   BSC8.unpack input ++ "\n"

messageGetError :: BSC8.ByteString -> String -> IO ()
messageGetError onInput err = do
  appendFile logFile $ "\n\nrmcParseError:|" ++ BSC8.unpack onInput ++ 
                       "|\n  With error:" ++ err
