{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, TemplateHaskell #-}
module Tuple.TupleApply
	where

import Tuple.TupleJoin
import Tools.TemplateTools(tapplyInstances)

import Data.Tuple.All

class TupleApply t inp outp | t -> outp, t -> inp, inp outp -> t where
    tapply :: t -> inp -> outp
    -- ^ apply each function in input tuple to input in order to produce output tuple

$(tapplyInstances 20)
