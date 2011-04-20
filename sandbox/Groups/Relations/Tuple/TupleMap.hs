{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, TemplateHaskell #-}

module Tuple.TupleMap where

import RelMap
import Tools.TemplateTools
import Data.Tuple.All

class TupleMap fn t1 t2 where
    tmap :: fn -> t1 -> t2

$(tmapConstructorInstances ([1,2], Left [1,2]) 1)

{-
instance TupleMap (f a1 a2 -> g a1 a2) 	(f a1 a2, f a1 a2) (g a1 a2, g a1 a2) where
    tmap f (a1, a2) = (f a1, f a2)
-}

{-
instance TupleMap (a -> a) (b, c) (b, c) where

instance TupleMap (f a1 a2 -> g a1 a2) 	(f b1_1 b1_2, f b2_1 b2_2) (g b1_1 b1_2, g b2_1 b2_2) where
instance TupleMap (f a1 a2 -> g a1) 	(f b1 b2, f c) (g b, g c) where
instance TupleMap (f a1 a2 -> g a2) 	(f b, f c) (g b, g c) where
instance TupleMap (f a1 -> g a1) 	(f b, f c) (g b, g c) where
instance TupleMap (f a1 -> a1) 		(f b, f c) (g b, g c) where
-}

