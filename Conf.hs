module Conf(getNParser, getParsers, getNAgreg, getAgregs,
            nodeInput, nodeOutput) where

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Control.Monad
import Utils

-- NParser - node parser
data Node = NParser { nodeInput		:: String,
                      nodeOutput	:: String
                    } |

            NAgreg  { nodeOutput	:: String
                    }
            deriving (Show, Read) 

isNParser (NParser _ _) = True
isNParser _		= False

isNAgreg (NAgreg _)	= True
isNAgreg _		= False

{- this module defines access to config -}

-- FIXME conf.cfg will be read each call of getMaybeAddr
-- FIXME silly, unflexible format conf data
confFile = "conf.cfg"

readConf :: IO [Node]
readConf = do
  cnt <- readFile confFile
  case maybeRead $ tail $ dropWhile (/= '}') cnt of
    Just x  -> return x
    Nothing -> Utils.exitWithErr $ "parse error:" ++ confFile

getNodes :: (Node -> Bool) -> IO [Node]
getNodes filterBy = readConf >>= (return . filter filterBy)

getNode :: (Node -> Bool) -> Int -> IO Node
getNode filterBy n = readConf >>= (return . tryIndex n . filter filterBy)

{- with exception handling -}
getNParser, getNAgreg :: Int -> IO Node
getParsers, getAgregs :: IO [Node]

getNParser = getNode isNParser
getNAgreg  = getNode isNAgreg

getParsers = getNodes isNParser
getAgregs  = getNodes isNAgreg

tryIndex :: Int -> [a] -> a
tryIndex n list | n < 0 || n >= length list = error $ "parse error:" ++ confFile
                | otherwise		    = list !! n
