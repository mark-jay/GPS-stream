{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, FunctionalDependencies #-}
-- {-# LANGUAGE UndecidableInstances #-}
	

module Main where

import TupleApply
import TupleJoin
import Relation
-- import RelMap
import TupleConcat

import Control.Applicative
import Control.Arrow
import Control.Monad hiding (join)

import Data.Tuple.All
import Data.Foldable

import qualified Data.Maybe as Maybe

import Prelude hiding (foldl, sum, head)
import qualified Prelude

--------------------------------------------------------------------------------

{-
-- deprt >= val 10

(fn1 `less` fn2) a = fn1 a < fn2 a
(fn1 `more` fn2) a = fn1 a < fn2 a
-- ... etc
-- Where  (employee_id `more` val 10)
-}

--------------------------------------------------------------------------------

data Employee = Employee { employee_id	 	:: Int
                         , employee_name 	:: String
                         , deprt		:: Int
                         }
              deriving(Show, Read, Eq, Ord)

newtype EmplPK = EmplPK Int
    deriving(Show, Read, Eq, Ord)

toPK :: Employee -> EmplPK
toPK = EmplPK . employee_id

employees' :: [Employee]
employees' = [(Employee 1 "1" 10), (Employee 2 "2" 20), (Employee 3 "3" 20)]

employees :: [Row EmplPK Employee]
employees  = map fn employees'
    where fn empl = Row (toPK empl) empl

--------------------
-- FIXME

{-
select :: (??? t inp outp) => t -> RelMap inp outp
select selectors = undefined -- projection (tapply selectors)
-- select (employee_id, employee_name) employees'
-}
