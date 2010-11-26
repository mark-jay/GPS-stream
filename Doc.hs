module Doc where

import System.Exit
import System.IO
import Control.Monad

exitWithErr errMessage = do hPutStrLn stderr errMessage
                            exitFailure

docAndExit msg = do putStrLn msg
                    exitSuccess

wrongNumArgs = "invalid number of arguments(try --help)"

lengthArgsCheck args n = do when (length args <= n) $ 
                              Doc.exitWithErr Doc.wrongNumArgs

helpInArgsCheck args helpString = do when ("--help" `elem` args) $
                                       docAndExit helpString
