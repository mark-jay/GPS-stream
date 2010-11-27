module Doc where

import System.Exit
import System.IO
import Control.Monad
import qualified Data.Maybe as Maybe
import qualified Utils
import qualified Conf

{- this module consist error messages, 
   assertions, documentation etc -}

exitWithErr errMessage = do hPutStrLn stderr errMessage
                            exitFailure

docAndExit msg = do putStrLn msg
                    exitSuccess

wrongNumArgs = "invalid number of arguments(try --help)"

lengthArgsAssert cond = do when (not cond) $
                             exitWithErr wrongNumArgs

helpInArgsCheck args helpString = do when ("--help" `elem` args) $
                                       docAndExit helpString

addressExistAssert string n = do addr <- Conf.getMaybeAddr string n
                                 when (Maybe.isNothing addr) $
                                   exitWithErr $ "address " ++ string ++ ":" ++ show n ++ "does not exist"

naturalNumAssert string err = if (reads :: ReadS Int) string /= []
                              then return ()
                              else exitWithErr err