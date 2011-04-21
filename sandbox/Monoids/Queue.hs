module Queue ( flattenQ
             , Queue(..)
             , qunion
             , qsingleton
             )where

import qualified Data.Sequence as Seq
import Data.Sequence(Seq, (|>), (><))

import qualified Data.FingerTree as FT
import Data.FingerTree(FingerTree, Measured)

import Data.Foldable
import Control.Arrow

import Prelude hiding(foldr, foldl)

class Queue f where
    qempty :: f a
    qpush  :: f a -> a -> f a
    qpop   :: f a -> (f a, a)

instance Queue Seq where
    qempty = Seq.empty
    qpush  = (|>)
    qpop   = f . Seq.viewl
        where f Seq.EmptyL 	= error "qpop: empty queue"
              f (el Seq.:< acc) = (acc, el)

instance Queue [] where
    qempty = []
    list `qpush` el  = list ++ [el]
    qpop = tail &&& head

qunion :: (Queue f, Foldable f) => f a -> f a -> f a
qunion fa1 fa2 = foldl qpush fa1 fa2
{-# RULES "qunion/Seq" qunion = (Seq.><) #-}

flattenQ :: (Queue f, Foldable f) =>
            f (f a) -> f a
flattenQ = foldr qunion qempty

qsingleton :: (Queue f) => a -> f a
qsingleton = qpush qempty