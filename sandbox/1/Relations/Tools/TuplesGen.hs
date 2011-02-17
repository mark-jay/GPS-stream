{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, PatternGuards #-}

import System.Environment

import Control.Monad

import qualified Data.List as List

-- 'join' depends on 'union'
main = do
  args <- getArgs
  main' args
    where
      main' args@[cmd, n] | Just fn <- getCommand cmd	= success fn (read n) args
                          | otherwise			= error "bad args"
      success fn n args = do
	      putStrLn $ "-- generated code 'Tools.TuplesGen.Main' was called with args '" ++
		         List.intercalate " " args ++ "'"
	      fn n

getCommand :: String   -> Maybe (Int -> IO ())
getCommand "union" 	= Just unionN
getCommand "join"	= Just tjoinN
getCommand "apply" 	= Just applyN
getCommand "time" 	= Just timeN
getCommand _ 		= Nothing

vars :: String -> [String]
vars name = map ((name ++) . show) [1..]

as  	= vars "a"
outps 	= vars "outp"
fns   	= vars "fn"

parWrap :: String -> String
parWrap a   = "(" ++ a ++ ")"

tuple :: [String] -> String
tuple []    = "()"
tuple [arg] = "(OneTuple (" ++ arg ++ "))"
tuple args  = parWrap . List.intercalate ", " $ args

unionN :: Int -> IO ()
unionN n = mapM_ unionN' [(i, j) | i <- [0..n], j <- [0..n], i+j <= n]
    where unionN' (n, m) = do
	    putStrLn $ "instance TupleUnion " ++ List.intercalate " " [t1, t2, t3] ++ " where" ++
                       "\n    tunion " ++ t1 ++ " " ++ t2 ++ " = " ++ t3
		where
		  (t1, t2, t3)  = (tuple vars1, tuple vars2, tuple vars3)
		  vars1         = take n  $ as
		  vars2		= take m . drop n $ as
		  vars3		= vars1 ++ vars2

applyN :: Int -> IO ()
applyN n = mapM_ applyN' [1 .. n]
    where applyN' n = do
            putStrLn $ "instance TupleApply " ++ inpTupleType ++
                       " inp " ++ outTupleType ++ " where\n    " ++
                       "tapply " ++ inpTuple ++ " inp = " ++ outpTuple
                where inpTupleType = tuple $ map ("inp->"++) $ take n outps
                      outTupleType = tuple $ take n outps
                      inpTuple     = tuple $ take n fns
                      outpTuple    = tuple $ map (++" inp") $ take n fns

tjoinN :: Int -> IO ()
tjoinN n = mapM_ tjoinN' [2 .. n]
    where tjoinN' n = do
            putStrLn $ "instance (Relation r, Eq k) => TupleJoin\n" ++
                       "    " ++ tuple (map (\x->"r (Row k "++x++")") args) ++ "\n" ++
                       "    " ++ "(r (Row k " ++ tuple args ++ "))\n" ++
                       "        where\n" ++
                       "    " ++ "tjoin' " ++ tuple rkas ++ " = " ++ solution
                where
                  args			= take n as
                  rkas			= take n $ vars "rka"
                  solution | n > 2	= List.intercalate " $ " (map ("joinByKey sndInsTuple "++) f) ++
                                          " $ joinByKey sndMkPair " ++ l1 ++ " " ++ l2
                           | otherwise  = "joinByKey sndMkPair " ++ l1 ++ " " ++ l2
                  (f, [l1,l2])	= splitAt (n-2) rkas

timeN :: Int -> IO ()
timeN n = mapM_ timeN' [1 .. n]
    where timeN' n = do
            let args = "UTCTime" : take n as
                t    = tuple args
            putStrLn $ "instance Timable " ++ t ++ " where\n" ++
                       "    toTimeVal = sel1"
