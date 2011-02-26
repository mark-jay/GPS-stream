{-# LANGUAGE RankNTypes, FlexibleContexts, FlexibleInstances, TupleSections #-}
module Window
    ( windowedAggrMap
    , windowedAggrSeq
    , windowedFn
    )
    where

--------------------

import Data.Set(Set)
import qualified Data.Set as Set
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Sequence(Seq, (|>), (<|), ViewR(..), ViewL(..))
import qualified Data.Sequence as Seq

import Data.Monoid
import Data.Time
import Data.Time.Clock
import Data.Tuple.All

import Control.Arrow
import Control.Monad

import Undable
import Timable
import RelMap

--------------------------------------------------------------------------------

class Container c where
    cEmpty   :: c a
    cInsert  :: (Timable a) => a -> c a -> c a
    cSize    :: c a -> Int
    cGetLast :: c a -> Maybe (c a, a)

instance Container Seq where
    cEmpty	= Seq.empty
    cInsert	= flip (|>)
    cSize	= Seq.length
    cGetLast	= fn . Seq.viewl
        where fn EmptyL 	= Nothing
              fn (el :< sequ) 	= Just (sequ, el)

instance Container (Map UTCTime) where
    cEmpty	= Map.empty
    cInsert v	= v `seq` Map.insert (toTime v) v
    cSize	= Map.size
    cGetLast	= liftM swap . Map.minView
        where swap (a, b) = (b, a)

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

type WindowedFn a b = (Timable a, Undable b, Ord b) =>
    Maybe Int -> Maybe NominalDiffTime -> (a -> b) -> RelMap a b

windowedAggrMap :: WindowedFn a b
windowedAggrMap = windowedAggr (cEmpty :: Map UTCTime a)

windowedAggrSeq :: WindowedFn a b
windowedAggrSeq = windowedAggr (cEmpty :: Seq a)

windowedFn :: (Timable inp) => Maybe Int -> Maybe NominalDiffTime -> RelMap inp inp

--------------------
windowedAggr :: (Timable a, Undable b, Container c, Ord b) =>
                c (UTCTime, b)		-- emptyContainer to fix it's type
             -> Maybe Int		-- maxRows
             -> Maybe NominalDiffTime	-- maxInterval
             -> (a -> b)		-- selector
             -> RelMap a b
windowedAggr contEmpty cnt interv sel = fromFn (toTime &&& sel) >>> windower >>> fromFn sel3
    where
      lim' = lim cnt interv
      windower = lim' `seq` foldyRM fn (minBound :: UTCTime, contEmpty, mempty)
      fn rv@(rowTime, val) (curTime, cont, mon) = mon' `seq` (curTime', cont', mon')
          where
            mon' = foldl rmappend (mon `mappend` val) elsToRm

            curTime' = rowTime `max` curTime

            (elsToRm, cont') = lim' curTime' $ rv `cInsert` cont

windowedFn cnt interv = windower >>> fromFn sel2
    where
      lim' = lim cnt interv
      emp = Seq.empty
      windower = RM (mkMapper minBound emp) emp
      mkMapper curTime acc el = RM (mkMapper curTime' acc') acc'
          where curTime' = curTime `max` rowTime
                rowTime  = toTime el
                (_,acc') = lim' curTime' $ (rowTime, el) `cInsert` acc

-- optimizable
lim ::
    (Container c) => Maybe Int -> Maybe NominalDiffTime
    -> UTCTime -> c (UTCTime, a) -> ([a], c (UTCTime, a))
lim Nothing 	Nothing        _        c = ([], c)
lim (Just cnt) 	Nothing        _        c = countLim cnt c
lim Nothing 	(Just interv)  curTime' c = timeLim curTime' interv c
lim (Just cnt)  (Just interv)  curTime' c =
    let (elms,  restCont ) = countLim cnt c
        (elms', restCont') = timeLim curTime' interv restCont
    in (elms ++ elms', restCont')

-- FIXME, too slow
timeLim :: (Container c) => UTCTime -> NominalDiffTime -> c (UTCTime, b) -> ([b], c (UTCTime, b))
timeLim curTime interv c = f (cGetLast c)
    where f Nothing = ([], c)
          f (Just (cuttedCont, (rTime, el)))
              | curTime `diffUTCTime` rTime > interv =
                  first (el:) $ timeLim curTime interv cuttedCont
              | otherwise = ([], c)

countLim :: (Container c) => Int -> c (UTCTime, b) -> ([b], c (UTCTime, b))
countLim cnt c | cSize c > cnt = countLim' (cGetLast c)
               | otherwise     = ([], c)
    where countLim' Nothing				= ([], c)
          countLim' (Just (cuttedCont, (_, el))) 	= ([el], cuttedCont)


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
{- remove it all! Time tests -}


timeM :: (Show a) => IO a -> IO ()
timeM a = do
  t <- getCurrentTime
  a >>= print
  t' <- getCurrentTime
  print (t' `diffUTCTime` t)

--------------------------------------------------------------------------------

mkTimedList :: (Num a, Enum a) => Int -> IO [(UTCTime, a)]
mkTimedList n = mapM fn (take n [1..])
    where fn n = getCurrentTime >>= return . (,n)

-- by count filter(~ 0.258237s)
main :: IO ()
main = do
  let rowToProc = Just 20
      t = windowedAggrSeq rowToProc Nothing (Product . sel2)
      test  x = do
         timedList <- mkTimedList x
         return $ feedElms t timedList
  timeM $ test 10000
  return ()

-- by time filter (~0.918319s)
main1 :: IO ()
main1 = do
  let rowToProc = Just 20
      test  x = do
          timedList <- mkTimedList x
          let diff = fst (timedList !! 100) `diffUTCTime` fst (timedList !! 0)
              t = windowedAggrSeq Nothing (Just diff) (mkCount . sel2)
          print diff
          return $ feedElms t timedList
  timeM $ test 10000
  return ()

{-
windowedAggr :: (Timable a, Undable b, Container c, Ord b) =>
                c (UTCTime, b)		-- emptyContainer to fix it's type
             -> Maybe Int		-- maxRows
             -> Maybe NominalDiffTime	-- maxInterval
             -> (a -> b)		-- selector
             -> RelMap a b

range (c, t) 	= windowedAggrMap c t
row c 		= (Just c, Nothing)
interval t 	= (Nothing, Just t)
previous  	= id
-}