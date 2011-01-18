module SqlStream.TableMap(TableMap(TableMap), 
                          elems, fromList, feedList,
                          mkTMap, mkTMap', mkTMapWithKeys, mkTMapWithKeys',
                          filter, map, mapFilter,
                          mapFst, mapSnd, (><),
                          avrg, sum, count)
    where

import qualified Data.Map as Map 

import Control.Monad
import Control.Arrow
import Control.Monad.State
import Control.Category

import Prelude hiding((.), id, filter, map, sum)

-- | This module aimed to be qualified
--   import SqlStream.TableMap (TableMap)
--   import qualified SqlStream.TableMap as TableMap

--------------------------------------------------------------------------------
------------  data definition, constructors and selectors   --------------------
--------------------------------------------------------------------------------
data TableMap key inp outp = 
    TableMap { qFeed		:: inp -> TableMap key inp outp
             , qAccum		:: Either [outp] (Map.Map key outp)	-- list for effectiv. only
             }

----------------------------------------
elems :: TableMap key inp outp -> [outp]
elems queryS = case qAccum queryS of
                     Left  outps 	-> outps
                     Right map		-> Map.elems map

fromList :: [inoutp] -> TableMap key inoutp inoutp
fromList = feedList id

----------------------------------------
mkTMapWithKeys :: (Ord key) =>
           (inp -> (key, outp))
        -> (outp -> outp -> outp)
        -> TableMap key inp (key, outp)
mkTMapWithKeys toKeyVal foldFn = TableMap (mkFeed acc) (Right acc)
    where
      acc = Map.empty

      mkFeed acc inp = TableMap (mkFeed newAcc) (Right newAcc)
          where newAcc | key `Map.member` acc 	= upd
                       | otherwise		= ins
                upd = Map.update (Just . (mapSnd $ foldFn value)) key acc

                ins = Map.insert key (key, value) acc

                (key, value) = toKeyVal inp

mkTMap :: (Ord key) =>
           (inp -> (key, outp))
        -> (outp -> outp -> outp)
        -> TableMap key inp outp
mkTMap toKeyVal foldFn = mkTMapWithKeys toKeyVal foldFn >>> 
                          map (\(k, v) -> v)
----------------------------------------
mapInsertOrAlterWithKeys :: (Ord key) => key
                 -> val'
                 -> val
                 -> (val -> val' -> val)
                 -> Map.Map key (key, val)
                 -> Map.Map key (key, val)
--- insert (v `f` zeroV) into Map or ---
--- update Map with (v `f` currV)    ---
mapInsertOrAlterWithKeys key v zeroV fn map 
    | key `Map.member` map 	= Map.update upd key map
    | otherwise			= Map.insert key (key, (fn zeroV v)) map
    where upd = Just . mapSnd (flip fn v)


mkTMapWithKeys' :: (Ord key) => (inp -> (key, outp'))
         -> (outp -> outp' -> outp)
         -> outp
         -> TableMap key inp (key, outp)
-- same that mkTMap' but with zero element --
mkTMapWithKeys' toKeyVal foldFn zeroEl = TableMap (mkFeed acc) (Right acc)
    where
      acc = Map.empty

      mkFeed acc inp = TableMap (mkFeed newAcc) (Right newAcc)
          where newAcc = mapInsertOrAlterWithKeys key value zeroEl foldFn acc

                (key, value) = toKeyVal inp

mkTMap' :: (Ord key) => (inp -> (key, outp'))
         -> (outp -> outp' -> outp)
         -> outp
         -> TableMap key inp outp
-- same that mkTMap' but with zero element --
mkTMap' toKeyVal foldFn zeroV = mkTMapWithKeys' toKeyVal foldFn zeroV >>> 
                                 map (\(k, v) -> v)

----------------------------------------
mapFilter :: (a -> b) -> (a -> Bool) -> TableMap key a b
mapFilter mapper pred = TableMap (mkFeed acc) acc where
    acc = Left []
    mkFeed acc el = TableMap (mkFeed newAcc) newAcc
        where newAcc | pred el 	 = Left $ mapper el : list
                     | otherwise = acc
              Left list = acc

filter 	pred = mapFilter id pred
map 	fn   = mapFilter fn (const True)

--------------------------------------------------------------------------------
-----------------------    instances       -------------------------------------
--------------------------------------------------------------------------------
instance Category (TableMap key) where
    id  = idQS
    (.) = composeQS

-- assoc composition
composeQS :: TableMap k b c -> TableMap k1 a b -> TableMap k a c
q2@(TableMap f2 acc2) `composeQS` q1@(TableMap f1 acc1) = TableMap f3 acc3 
    where
      (TableMap _ acc3) = feedList q2 $ elems q1

      f3 el		= q2 `composeQS` (f1 el)

idQS :: TableMap key inoutp inoutp
idQS = filter (const True)

--------------------------------------------------------------------------------
-----------------------    another API     -------------------------------------
--------------------------------------------------------------------------------

feedList :: TableMap key inp outp -> [inp] -> TableMap key inp outp
feedList q [] 		= q
feedList q (inp:inps) 	= feedList (qFeed q inp) inps

-- for -withKey functions
mapValue :: (a -> b) -> TableMap key (key, a) (key, b)
mapValue fn = map (mapSnd fn)
--------------------------------------------------------------------------------
-----------------------       utils        -------------------------------------
--------------------------------------------------------------------------------

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (x, y) = (f x, y)

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (x, y) = (x, f y)

infixr 8 ><
(><) :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
(f >< g) (x,y) = (f x,g y)

--------------------------------------------------------------------------------
-- examples:

sumCountPair toKey = mkTMapWithKeys' (toKey &&& id) foldFn (0, 0)
    where 
      foldFn (sum, count) el 	= (sum + el, count + 1)

avrg	toKey = sumCountPair toKey >>> mapValue (uncurry (/))
sum	toKey = sumCountPair toKey >>> mapValue fst
count	toKey = sumCountPair toKey >>> mapValue snd

--------------------------------------------------------------------------------
-- examples:

-- Example:
--   let q = mkTMapWithKeys ((`rem` 3) &&& id) (+)
--   elems $ fromList [0..20] >>> q
--   elems $ fromList [0..20] >>> q >>> map (+1) >>> map (+1) >>> filter (/= 65)

--   elems $ mkAvrg (const []) <<< fromList [0..20]

--------------------------------------------------------------------------------
