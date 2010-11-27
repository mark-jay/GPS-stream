module Conf where

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Control.Monad

{- this module defines access to config -}
confFile = "conf.cfg"

getMaybeAddr :: String -> Int -> IO (Maybe String)
getMaybeAddr section number = do cnt <- readFile confFile 
                                 let map' = (read :: String -> Map.Map String [String]) cnt
                                 return $ (Map.lookup section map') >>= maybeIndex number

maybeIndex :: Int -> [a] -> Maybe a
maybeIndex n list | n < 0 || n >= length list = Nothing
                  | otherwise		      = Just $ list !! n

getAddr :: String -> Int -> IO String
getAddr section = liftM Maybe.fromJust . getMaybeAddr section