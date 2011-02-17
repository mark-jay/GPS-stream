{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
module TupleApply
	where

import Data.Tuple.All
import TupleJoin

class TupleApply t inp outp | t -> outp, t -> inp, inp outp -> t where
    -- Minimal complete definition:
    --      tapply
    tapply :: t -> inp -> outp

-- generated code 'Tools.TuplesGen.Main' was called with args 'apply 20'
instance TupleApply (OneTuple (inp->outp1)) inp (OneTuple (outp1)) where
    tapply (OneTuple (fn1)) inp = (OneTuple (fn1 inp))
instance TupleApply (inp->outp1, inp->outp2) inp (outp1, outp2) where
    tapply (fn1, fn2) inp = (fn1 inp, fn2 inp)
instance TupleApply (inp->outp1, inp->outp2, inp->outp3) inp (outp1, outp2, outp3) where
    tapply (fn1, fn2, fn3) inp = (fn1 inp, fn2 inp, fn3 inp)
instance TupleApply (inp->outp1, inp->outp2, inp->outp3, inp->outp4) inp (outp1, outp2, outp3, outp4) where
    tapply (fn1, fn2, fn3, fn4) inp = (fn1 inp, fn2 inp, fn3 inp, fn4 inp)
instance TupleApply (inp->outp1, inp->outp2, inp->outp3, inp->outp4, inp->outp5) inp (outp1, outp2, outp3, outp4, outp5) where
    tapply (fn1, fn2, fn3, fn4, fn5) inp = (fn1 inp, fn2 inp, fn3 inp, fn4 inp, fn5 inp)
instance TupleApply (inp->outp1, inp->outp2, inp->outp3, inp->outp4, inp->outp5, inp->outp6) inp (outp1, outp2, outp3, outp4, outp5, outp6) where
    tapply (fn1, fn2, fn3, fn4, fn5, fn6) inp = (fn1 inp, fn2 inp, fn3 inp, fn4 inp, fn5 inp, fn6 inp)
instance TupleApply (inp->outp1, inp->outp2, inp->outp3, inp->outp4, inp->outp5, inp->outp6, inp->outp7) inp (outp1, outp2, outp3, outp4, outp5, outp6, outp7) where
    tapply (fn1, fn2, fn3, fn4, fn5, fn6, fn7) inp = (fn1 inp, fn2 inp, fn3 inp, fn4 inp, fn5 inp, fn6 inp, fn7 inp)
instance TupleApply (inp->outp1, inp->outp2, inp->outp3, inp->outp4, inp->outp5, inp->outp6, inp->outp7, inp->outp8) inp (outp1, outp2, outp3, outp4, outp5, outp6, outp7, outp8) where
    tapply (fn1, fn2, fn3, fn4, fn5, fn6, fn7, fn8) inp = (fn1 inp, fn2 inp, fn3 inp, fn4 inp, fn5 inp, fn6 inp, fn7 inp, fn8 inp)
instance TupleApply (inp->outp1, inp->outp2, inp->outp3, inp->outp4, inp->outp5, inp->outp6, inp->outp7, inp->outp8, inp->outp9) inp (outp1, outp2, outp3, outp4, outp5, outp6, outp7, outp8, outp9) where
    tapply (fn1, fn2, fn3, fn4, fn5, fn6, fn7, fn8, fn9) inp = (fn1 inp, fn2 inp, fn3 inp, fn4 inp, fn5 inp, fn6 inp, fn7 inp, fn8 inp, fn9 inp)
instance TupleApply (inp->outp1, inp->outp2, inp->outp3, inp->outp4, inp->outp5, inp->outp6, inp->outp7, inp->outp8, inp->outp9, inp->outp10) inp (outp1, outp2, outp3, outp4, outp5, outp6, outp7, outp8, outp9, outp10) where
    tapply (fn1, fn2, fn3, fn4, fn5, fn6, fn7, fn8, fn9, fn10) inp = (fn1 inp, fn2 inp, fn3 inp, fn4 inp, fn5 inp, fn6 inp, fn7 inp, fn8 inp, fn9 inp, fn10 inp)
instance TupleApply (inp->outp1, inp->outp2, inp->outp3, inp->outp4, inp->outp5, inp->outp6, inp->outp7, inp->outp8, inp->outp9, inp->outp10, inp->outp11) inp (outp1, outp2, outp3, outp4, outp5, outp6, outp7, outp8, outp9, outp10, outp11) where
    tapply (fn1, fn2, fn3, fn4, fn5, fn6, fn7, fn8, fn9, fn10, fn11) inp = (fn1 inp, fn2 inp, fn3 inp, fn4 inp, fn5 inp, fn6 inp, fn7 inp, fn8 inp, fn9 inp, fn10 inp, fn11 inp)
instance TupleApply (inp->outp1, inp->outp2, inp->outp3, inp->outp4, inp->outp5, inp->outp6, inp->outp7, inp->outp8, inp->outp9, inp->outp10, inp->outp11, inp->outp12) inp (outp1, outp2, outp3, outp4, outp5, outp6, outp7, outp8, outp9, outp10, outp11, outp12) where
    tapply (fn1, fn2, fn3, fn4, fn5, fn6, fn7, fn8, fn9, fn10, fn11, fn12) inp = (fn1 inp, fn2 inp, fn3 inp, fn4 inp, fn5 inp, fn6 inp, fn7 inp, fn8 inp, fn9 inp, fn10 inp, fn11 inp, fn12 inp)
instance TupleApply (inp->outp1, inp->outp2, inp->outp3, inp->outp4, inp->outp5, inp->outp6, inp->outp7, inp->outp8, inp->outp9, inp->outp10, inp->outp11, inp->outp12, inp->outp13) inp (outp1, outp2, outp3, outp4, outp5, outp6, outp7, outp8, outp9, outp10, outp11, outp12, outp13) where
    tapply (fn1, fn2, fn3, fn4, fn5, fn6, fn7, fn8, fn9, fn10, fn11, fn12, fn13) inp = (fn1 inp, fn2 inp, fn3 inp, fn4 inp, fn5 inp, fn6 inp, fn7 inp, fn8 inp, fn9 inp, fn10 inp, fn11 inp, fn12 inp, fn13 inp)
instance TupleApply (inp->outp1, inp->outp2, inp->outp3, inp->outp4, inp->outp5, inp->outp6, inp->outp7, inp->outp8, inp->outp9, inp->outp10, inp->outp11, inp->outp12, inp->outp13, inp->outp14) inp (outp1, outp2, outp3, outp4, outp5, outp6, outp7, outp8, outp9, outp10, outp11, outp12, outp13, outp14) where
    tapply (fn1, fn2, fn3, fn4, fn5, fn6, fn7, fn8, fn9, fn10, fn11, fn12, fn13, fn14) inp = (fn1 inp, fn2 inp, fn3 inp, fn4 inp, fn5 inp, fn6 inp, fn7 inp, fn8 inp, fn9 inp, fn10 inp, fn11 inp, fn12 inp, fn13 inp, fn14 inp)
instance TupleApply (inp->outp1, inp->outp2, inp->outp3, inp->outp4, inp->outp5, inp->outp6, inp->outp7, inp->outp8, inp->outp9, inp->outp10, inp->outp11, inp->outp12, inp->outp13, inp->outp14, inp->outp15) inp (outp1, outp2, outp3, outp4, outp5, outp6, outp7, outp8, outp9, outp10, outp11, outp12, outp13, outp14, outp15) where
    tapply (fn1, fn2, fn3, fn4, fn5, fn6, fn7, fn8, fn9, fn10, fn11, fn12, fn13, fn14, fn15) inp = (fn1 inp, fn2 inp, fn3 inp, fn4 inp, fn5 inp, fn6 inp, fn7 inp, fn8 inp, fn9 inp, fn10 inp, fn11 inp, fn12 inp, fn13 inp, fn14 inp, fn15 inp)
instance TupleApply (inp->outp1, inp->outp2, inp->outp3, inp->outp4, inp->outp5, inp->outp6, inp->outp7, inp->outp8, inp->outp9, inp->outp10, inp->outp11, inp->outp12, inp->outp13, inp->outp14, inp->outp15, inp->outp16) inp (outp1, outp2, outp3, outp4, outp5, outp6, outp7, outp8, outp9, outp10, outp11, outp12, outp13, outp14, outp15, outp16) where
    tapply (fn1, fn2, fn3, fn4, fn5, fn6, fn7, fn8, fn9, fn10, fn11, fn12, fn13, fn14, fn15, fn16) inp = (fn1 inp, fn2 inp, fn3 inp, fn4 inp, fn5 inp, fn6 inp, fn7 inp, fn8 inp, fn9 inp, fn10 inp, fn11 inp, fn12 inp, fn13 inp, fn14 inp, fn15 inp, fn16 inp)
instance TupleApply (inp->outp1, inp->outp2, inp->outp3, inp->outp4, inp->outp5, inp->outp6, inp->outp7, inp->outp8, inp->outp9, inp->outp10, inp->outp11, inp->outp12, inp->outp13, inp->outp14, inp->outp15, inp->outp16, inp->outp17) inp (outp1, outp2, outp3, outp4, outp5, outp6, outp7, outp8, outp9, outp10, outp11, outp12, outp13, outp14, outp15, outp16, outp17) where
    tapply (fn1, fn2, fn3, fn4, fn5, fn6, fn7, fn8, fn9, fn10, fn11, fn12, fn13, fn14, fn15, fn16, fn17) inp = (fn1 inp, fn2 inp, fn3 inp, fn4 inp, fn5 inp, fn6 inp, fn7 inp, fn8 inp, fn9 inp, fn10 inp, fn11 inp, fn12 inp, fn13 inp, fn14 inp, fn15 inp, fn16 inp, fn17 inp)
instance TupleApply (inp->outp1, inp->outp2, inp->outp3, inp->outp4, inp->outp5, inp->outp6, inp->outp7, inp->outp8, inp->outp9, inp->outp10, inp->outp11, inp->outp12, inp->outp13, inp->outp14, inp->outp15, inp->outp16, inp->outp17, inp->outp18) inp (outp1, outp2, outp3, outp4, outp5, outp6, outp7, outp8, outp9, outp10, outp11, outp12, outp13, outp14, outp15, outp16, outp17, outp18) where
    tapply (fn1, fn2, fn3, fn4, fn5, fn6, fn7, fn8, fn9, fn10, fn11, fn12, fn13, fn14, fn15, fn16, fn17, fn18) inp = (fn1 inp, fn2 inp, fn3 inp, fn4 inp, fn5 inp, fn6 inp, fn7 inp, fn8 inp, fn9 inp, fn10 inp, fn11 inp, fn12 inp, fn13 inp, fn14 inp, fn15 inp, fn16 inp, fn17 inp, fn18 inp)
instance TupleApply (inp->outp1, inp->outp2, inp->outp3, inp->outp4, inp->outp5, inp->outp6, inp->outp7, inp->outp8, inp->outp9, inp->outp10, inp->outp11, inp->outp12, inp->outp13, inp->outp14, inp->outp15, inp->outp16, inp->outp17, inp->outp18, inp->outp19) inp (outp1, outp2, outp3, outp4, outp5, outp6, outp7, outp8, outp9, outp10, outp11, outp12, outp13, outp14, outp15, outp16, outp17, outp18, outp19) where
    tapply (fn1, fn2, fn3, fn4, fn5, fn6, fn7, fn8, fn9, fn10, fn11, fn12, fn13, fn14, fn15, fn16, fn17, fn18, fn19) inp = (fn1 inp, fn2 inp, fn3 inp, fn4 inp, fn5 inp, fn6 inp, fn7 inp, fn8 inp, fn9 inp, fn10 inp, fn11 inp, fn12 inp, fn13 inp, fn14 inp, fn15 inp, fn16 inp, fn17 inp, fn18 inp, fn19 inp)
instance TupleApply (inp->outp1, inp->outp2, inp->outp3, inp->outp4, inp->outp5, inp->outp6, inp->outp7, inp->outp8, inp->outp9, inp->outp10, inp->outp11, inp->outp12, inp->outp13, inp->outp14, inp->outp15, inp->outp16, inp->outp17, inp->outp18, inp->outp19, inp->outp20) inp (outp1, outp2, outp3, outp4, outp5, outp6, outp7, outp8, outp9, outp10, outp11, outp12, outp13, outp14, outp15, outp16, outp17, outp18, outp19, outp20) where
    tapply (fn1, fn2, fn3, fn4, fn5, fn6, fn7, fn8, fn9, fn10, fn11, fn12, fn13, fn14, fn15, fn16, fn17, fn18, fn19, fn20) inp = (fn1 inp, fn2 inp, fn3 inp, fn4 inp, fn5 inp, fn6 inp, fn7 inp, fn8 inp, fn9 inp, fn10 inp, fn11 inp, fn12 inp, fn13 inp, fn14 inp, fn15 inp, fn16 inp, fn17 inp, fn18 inp, fn19 inp, fn20 inp)
