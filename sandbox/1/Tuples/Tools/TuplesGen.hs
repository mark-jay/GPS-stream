{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, PatternGuards #-}

import System.Environment

import Control.Monad

import qualified Data.List as List

main = do
  args <- getArgs
  case args of
    [cmd, n] | Just fn <- lookup cmd commands 	-> success fn (read n) args
    _						-> error "bad args"
    where
      success fn n args = do
	      putStrLn $ "-- generated code 'Tools.TuplesGen.Main' was called with args '" ++ 
		         List.intercalate " " args ++ "'"
	      fn n
        
commands = [("union", unionN),
            ("apply", applyN)
           ]

vars name = map ((name ++) . show) [1..]
args  	= vars "a"
outps 	= vars "outp"
fns   	= vars "fn"

unionN n = mapM_ unionN' [(i, j) | i <- [1..n], j <- [1..n], i+j <= n]
    where unionN' (n, m) = do
	    putStrLn $ "instance TupleUnion " ++ List.intercalate " " [t1, t2, t3] ++ " where" ++ 
                       "\n    tunion " ++ t1 ++ " " ++ t2 ++ " = " ++ t3
		where 
		  (t1, t2, t3)  = (toTuple vars1, toTuple vars2, toTuple vars3)
		  toTuple [var] = "(OneTuple " ++ var ++ ")"
                  toTuple vars  = "(" ++ List.intercalate "," vars ++ ")"
		  vars1         = take n 		  $ args
		  vars2		= take m . drop n $ args
		  vars3		= vars1 ++ vars2

parWrap a   = "(" ++ a ++ ")"
tuple [arg] = "(OneTuple (" ++ arg ++ "))"
tuple args  = parWrap . List.intercalate ", " $ args

applyN n = mapM_ applyN' [1 .. n]
    where applyN' n = do
            putStrLn $ "instance TupleApply " ++ inpTupleType ++ 
                       " inp " ++ outTupleType ++ " where\n    " ++
                       "tapply " ++ inpTuple ++ " inp = " ++ outpTuple
                where inpTupleType = tuple $ map ("inp->"++) $ take n outps
                      outTupleType = tuple $ take n outps
                      inpTuple     = tuple $ take n fns
                      outpTuple    = tuple $ map (++" inp") $ take n fns
{-
instance TupleApply (OneTuple (inp->outp1)) inp (OneTuple outp1) where
    tapply (OneTuple fn1) inp = (OneTuple (fn1 inp))
-}
