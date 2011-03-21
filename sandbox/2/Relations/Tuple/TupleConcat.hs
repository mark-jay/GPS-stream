{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, TemplateHaskell #-}
module Tuple.TupleConcat(TupleConcat, tconcat, (|++|), (|++), (++|))
	where

import Data.Tuple.All

import Tools.TemplateTools(tconcatInstances)


class TupleConcat a b c | a b -> c where
    tconcat :: a -> b -> c
    -- ^ concatenate 2 tuple

(|++|)  :: (TupleConcat a b c) => a -> b -> c
(|++|)  =  tconcat

x ++| xs = (OneTuple x) |++| xs
xs |++ x = xs |++| (OneTuple x)

infixr 5 |++|

$(tconcatInstances 10)
