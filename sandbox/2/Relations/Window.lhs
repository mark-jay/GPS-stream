>{-# LANGUAGE RankNTypes, FlexibleContexts, FlexibleInstances, TupleSections, MultiParamTypeClasses, FunctionalDependencies, PatternGuards #-}
>module Window
>    ( windowAggr
>    , window
>    , lastSecs
>    , lastPico
>    , lastRows
>     )
>    where

>import Data.Set(Set)
>import qualified Data.Set as Set
>import Data.Map(Map)
>import qualified Data.Map as Map
>import Data.Sequence(Seq, (|>), (<|), ViewR(..), ViewL(..))
>import qualified Data.Sequence as Seq

>import Data.Maybe as Maybe
>import Data.Monoid
>import Data.Time
>import Data.Time.Clock
>import Data.Tuple.All

>import Control.Arrow
>import Control.Category
>import Control.Monad

>import Queue
>import Timable
>import RelMap
>import Aggrs

>import Prelude hiding (null, reverse, id, (.))

>import qualified Data.FingerTree as FT
>import Data.FingerTree(FingerTree, Measured)

>import Control.Applicative (Applicative(pure, (<*>)), (<$>))
>import Data.Monoid
>import Data.Max
>import Data.Foldable (Foldable(foldMap), toList)
>import Data.Traversable (Traversable(traverse))


Единственной целью типа-обертки Acc является сделать возможным хранения обернутого моноида в
FingerTree. FingerTree требует Measured, поэтому пара (UTCTime, Acc a) реализует Measured

>newtype Acc a = Acc { getAcc :: a }
>    deriving(Show)

>instance (Monoid a) => Measured (Max UTCTime, Sum Int, a) (UTCTime, Acc a) where
>    measure (t, a) = (Max t, Sum 1, getAcc a)

--------------------------------------
 Окна

window' принимает 
 -2 ограничителя: по времени и по количеству записей
 -функцию из записи в моноид

возвращаемое значение - это отображение отношений, которое хранит результат в FingerTree
Служебное поля FingerTree (Max UTCTime, Sum Int, b) позволяет быстро находить необходимые окна
исходя из текущего времени и количества записей.

Каждое полученное значение добавляется в аккумулятор, 
геттер аккумулятора - просто фильтрует дерево функцией restrictFT

ограничители представляют собой
 1)функцию от текущего времени во временные рамки
 2)функцию от количества в пару (начинаяСИндекса, количествоЗначений). Именно количествоЗначений, 
    а не заканчиваяИндексом, тк семантика restrictFT представляет собой (dropUntil _ . takeUntil _),
    а FingerTree пересчитывает моноид после разбиения (dropUntil и takeUntil это как раз разбиение)

>window'
>  :: (Timable a, Monoid b) =>
>     Maybe (UTCTime -> (UTCTime, UTCTime))
>  -> Maybe (Int -> (Int, Int))
>  -> (a -> b)
>  -> RelMapGen
>     (FingerTree (Max UTCTime, Sum Int, b)) a (UTCTime, Acc b)
>window' tRestr cRestr fn = restrictor
>    where
>      r = mkFTSelector fn

>      restrictFT' = restrictFT tRestr cRestr

>      restrictor = restrictFT' `seq` RM (restrMapper (rmMapper r))  (rmAcc r)
>      restrMapper mapper el = let RM f acc = mapper el
>                              in RM (restrMapper f) (restrictFT' acc)

windowAggr, window - две похожие функции, с той разницей, что 
windowAggr сворачивает результат(окно) по моноиде, соответственно
windowAggr в отличии от window имеет ограничение (Monoid b)

имея служебное поле FingerTree "(Max UTCTime, Sum Int, b)"
windowAggr обращается к "b" из служебного поля, который и содержит необходимый результат.
Т.к. каждое поддерево хранит частичный результат свертки моноида, 
пересчет результата при следующем запросе будет очень быстр.(быстрее чем за O(ln n))

Итого:
 добавления элемента - О(1), 
 выделение нужного окна не превысит O(ln n), 
    а точнее O(ln ((min (i, n) + min (n, j)))), 
    где n, i, j - размеры окна, остатка слева, остатка справа соответственно
 

>windowAggr
>  :: (Timable a, Monoid b) =>
>     Maybe (UTCTime -> (UTCTime, UTCTime))
>  -> Maybe (Int -> (Int, Int))
>  -> (a -> b)
>  -> RelMap a b
>windowAggr tRestr cRestr fn = fromFn sel3 . (selector $ window' tRestr cRestr fn)
>    where
>      selector r = RM (mkMapper r) (rToAcc r)
>          where
>            rToAcc = Seq.singleton . FT.measure . rmAcc
>            mkMapper r el = RM (mkMapper r') (rToAcc r')
>                where r' = rmMapper r el

window - просто ограничивает результат, не влияя на записи. чтобы вновь воспользоваться
функцией window' "b" нужно сделать моноидой, поэтому под эту роль сгодиться любой моноид, 
не имеющий ограничений на тип, например First.
Поэтому в функцию window' передадим композицию аргумента-функции и конструктора First,
а результат возьмем напрямую из Just.

>window :: (Timable a) =>
>          Maybe (UTCTime -> (UTCTime, UTCTime))
>       -> Maybe (Int     -> (Int, Int))
>       -> (a -> b)
>       -> RelMap a b
>window tRestr cRestr fn = selector `rmComp` window' tRestr cRestr (First . Just . fn)
>                          -- `rmComp` (fromFn (First . Just))
>    where selector :: RelMap (UTCTime, Acc (First b)) b
>          selector = fromFn f
>              where f (_, Acc (First (Just b))) = b



>mkFTSelector :: (Timable a, Monoid b) =>
>              (a -> b)
>           -> RelMapGen (FingerTree (Max UTCTime, Sum Int, b)) a (UTCTime, Acc b)
>mkFTSelector fn = RM (mkMapper FT.empty) FT.empty
>    where
>      f a = (toTime a, Acc $ fn a)
>      mkMapper acc el = RM (mkMapper acc') acc'
>          where acc' = acc FT.|> f el	-- FIXME? FT.|

--------------------------------------

restrictFT - функция, принимающая 2 ограничителя по времени и количеству,
возвращающая отображение FingerTree, а именно, ограничивает окном.

>restrictFT :: (Monoid b) =>
>              Maybe (UTCTime -> (UTCTime, UTCTime))	-- from/to time producer
>           -> Maybe (Int     -> (Int, Int))		-- from/to count producer
>           -> FingerTree (Max UTCTime, Sum Int, b) (UTCTime, Acc b)
>           -> FingerTree (Max UTCTime, Sum Int, b) (UTCTime, Acc b)
>restrictFT restrT restrC inpFT = (FT.takeUntil tPred . FT.dropUntil dPred $ inpFT)
>    where
>      (Max curTime, Sum count, _) = FT.measure inpFT

>      -- FIXME?
>      (minIdx, maxIdx)   = Maybe.fromMaybe (const (minBound, maxBound)) restrC $ count
>      (minTime, maxTime) = Maybe.fromMaybe (const (minBound, maxBound)) restrT $ curTime

>      -- take/drop while
>      tPred (Max time, Sum number, _) = not (time < maxTime && number < maxIdx)
>      dPred (Max time, Sum number, _) = not (time < minTime || number < minIdx)

------------------------------------------------------------------------------
Далее просто набор функций, для удобной работы,
способы построить ограничители из Integer, NominalDiffTime и тд

>lastSecs :: Integer -> Maybe (UTCTime -> (UTCTime, UTCTime))
>lastSecs n = Just f
>    where
>      f curTime = (minTime, curTime)
>          where minTime = addUTCTime (fromInteger (-n)) curTime

>lastPico :: NominalDiffTime -> Maybe (UTCTime -> (UTCTime, UTCTime))
>lastPico n = Just f
>    where
>      f curTime = (minTime, curTime)
>          where minTime = addUTCTime (-n) curTime

>lastRows :: (Num a, Bounded a) => a -> Maybe (a -> (a, a))
>lastRows n = Just f
>    where
>      f count = (minRow, maxBound)
>          where minRow = count - n + 1

------------------------------------------------------------------------------
функции для проверки производительности
все функции запускались на 10 000 входящих записях и отрабатывали быстрее чем за полсекунды.
в целом получилось быстрее, чем оптимизация группами. Кроме того для поиска нужного окна 
нужно около O(ln n), поэтому проверки могу быть достаточно ресурсоемкими и это не сильно 
повлияет на общую производительность.

Стоит также отметить, что mainAg1 не сильно отличается от mainAg2, по затраченному времени,
тк моноид, по которому происходила свертка считался лениво - т.е. при действительном запросе на свернутый моноид,
причем он посчитал свертки всех своих промежуточных узлов - поддеревьев, которые не придется 
считать при следующем обращении. Это дает прирост производительности при частых запросах, а не единичных запросах.
В качестве иллюстрации приведена функции mainAg2' и mainAg1', которые обрабатывают 20 000 данных и
выполняют запрос каждые 10 000. mainAg2' отработала за 0.623249s против mainAg1', отработавшей за
0.828081s


>rowToProc = 10000
>rowWind   = 50

>timeM :: (Show a) => IO a -> IO ()
>timeM a = do
>  t <- getCurrentTime
>  a >>= print
>  t' <- getCurrentTime
>  print (t' `diffUTCTime` t)

>mkTimedList :: (Num a, Enum a) => Int -> IO [(UTCTime, a)]
>mkTimedList n = mapM fn (take n [1..])
>    where fn n = getCurrentTime >>= return . (,n)

окно без агрегации по количеству записей (~0.40845s)

>test :: IO ()
>test = do
>  let t = window Nothing (lastRows rowWind) (Product . sel2)
>      test  x = do
>         timedList <- mkTimedList x
>         return $ feedElms t timedList
>  timeM $ test rowToProc
>  return ()

 агрегатор через композицию (~0.410508s)

>testAg1 :: IO ()
>testAg1 = do
>  let t = window Nothing (lastRows rowWind) (Product . sel2)
>      test  x = do
>         timedList <- mkTimedList x
>         return $ feedElms (foldyRM mappend mempty . t) timedList
>  timeM $ test rowToProc
>  return ()

 2 агрегатора через композицию (~0.828081s)

>testAg1' :: IO ()
>testAg1' = do
>  let t = window Nothing (lastRows rowWind) (Product . sel2)
>      test  x = do
>         timedList1 <- mkTimedList x
>         timedList2 <- mkTimedList x
>         let r1 = feedElms (foldyRM mappend mempty . t) timedList1
>             r2 = feedElms r1 timedList2
>         return (r1, r2)
>  timeM $ test rowToProc
>  return ()

 агрегатор через функцию windowAggr (~0.422327s)

>testAg2 :: IO ()
>testAg2 = do
>  let t = windowAggr Nothing (lastRows rowWind) (Product . sel2)
>      test  x = do
>         timedList <- mkTimedList x
>         return $ feedElms t timedList
>  timeM $ test rowToProc
>  return ()


 2 агрегатора через функцию windowAggr (~0.623249s)

>testAg2' :: IO ()
>testAg2' = do
>  let t = windowAggr Nothing (lastRows rowWind) (Product . sel2)
>      test  x = do
>         timedList1 <- mkTimedList x
>         timedList2 <- mkTimedList x
>         let r1 = feedElms t timedList1
>             r2 = feedElms r1 timedList1
>         return (r1,r2)
>  timeM $ test rowToProc
>  return ()

 окно без агрегации, по времени (~0.416505s)

>test1 :: IO ()
>test1 = do
>  let test  x = do
>          timedList <- mkTimedList x
>          let diff = fst (timedList !! rowWind) `diffUTCTime` fst (timedList !! 0)
>              t = window (lastPico diff) Nothing (Max *** Product)
>          print diff
>          return $ feedElms t timedList
>  timeM $ test rowToProc
>  return ()
