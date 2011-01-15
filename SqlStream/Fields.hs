module SqlStream.Fields(Field(..), fTruncate, fRound, fCeiling, fFloor) where

-- FIXME instances
import Data.Time
import Data.Int
import Data.Ratio
import Control.Monad
import qualified Data.List as List

import qualified RMC.Protobuf.RMC.RMC	 	as RMC
import qualified RMC.Protobuf.RMC.RMC	 	as RMC
import qualified RMC.Protobuf.RMC.RMC.MDeclDir	as RMC
import qualified RMC.Protobuf.RMC.RMC.Status	as RMC
import qualified RMC.Protobuf.RMC.RMC.ModeInd	as RMC

data Field = 
    FDouble 	{ fDouble	:: Maybe Double 	}
  | FDayTime 	{ fUTCTime	:: Maybe UTCTime 	}
  | FInt 	{ fInt		:: Maybe Int 		}
  | FInt32 	{ fInt32	:: Maybe Int32 		}
  | FRMCStatus	{ fRMCStatus	:: Maybe RMC.Status 	}
  | FMDeclDir 	{ fMDeclDir	:: Maybe RMC.MDeclDir 	}
  | FModeInd 	{ fModeInd	:: Maybe RMC.ModeInd 	}
    deriving(Show, Read, Eq)

badArgs args = "bad args: " ++ (List.intercalate ", " $ map show args)

mbPlus :: (Num a) => Maybe a -> Maybe a -> Maybe a
mbPlus  = liftM2 (+)

mbMult :: (Num a) => Maybe a -> Maybe a -> Maybe a
mbMult  = liftM2 (*)

fromIntegralMb :: (Integral a, Num b) => Maybe a -> Maybe b
fromIntegralMb  = liftM fromIntegral

instance Num Field where
  -- plus
  (FDouble a) + (FDouble b) = FDouble $ a `mbPlus` b
  (FInt a)    + (FInt b)    = FInt    $ a `mbPlus` b
  (FInt32 a)  + (FInt32 b)  = FInt32  $ a `mbPlus` b

  (FInt a)    + (FDouble b) = FDouble $ fromIntegralMb a `mbPlus` b
  (FDouble a) + (FInt b)    = FDouble $ a `mbPlus` fromIntegralMb b

  a           + b           = error $ "Field '+', " ++ badArgs [a,b]

  -- mult
  (FDouble a) * (FDouble b) = FDouble $ a `mbMult` b
  (FInt a)    * (FInt b)    = FInt    $ a `mbMult` b
  (FInt32 a)  * (FInt32 b)  = FInt32  $ a `mbMult` b

  (FInt a)    * (FDouble b) = FDouble $ fromIntegralMb a `mbMult` b
  (FDouble a) * (FInt b)    = FDouble $ a `mbMult` fromIntegralMb b

  a           * b           = error $ "Field '*', " ++ badArgs [a,b]

  -- fromInteger
  fromInteger	= FInt . Just . fromInteger

  (-)		= error "-       not implemented yet"
  negate	= error "negate  not implemented yet"
  abs		= error "abs     not implemented yet"
  signum	= error "signum  not implemented yet"

--------------------------------------------------------------------------------
instance Ord Field where
    compare (FInt a)     (FInt b)	= compare a b
    compare (FInt32 a)   (FInt32 b) 	= compare a b
    compare (FDouble a)  (FDouble b) 	= compare a b
    compare (FDayTime a) (FDayTime b) 	= compare a b

    compare (FInt a) 	 (FDouble b)	= compare (fromIntegralMb a) b
    compare (FDouble a)	 (FInt b)	= compare a (fromIntegralMb b)

    compare a b				= error $ "on ordering Fields: " ++ show a ++ show b

--------------------------------------------------------------------------------
instance Fractional Field where
    FDouble a	/ FDouble b		= FDouble (liftM2 (/) a b)
    a		/ b			= error $ "Field '/':" ++ show a ++ show b

    recip (FDouble a)			= FDouble (liftM recip a)
    recip a				= error $ "Field recip:" ++ show a

    fromRational 			= FDouble . Just . fromRational

--------------------------------------------------------------------------------
instance Real Field where
    toRational (FDouble (Just a))	= toRational a
    toRational (FInt    (Just a))	= toRational a
    toRational (FInt32  (Just a))	= toRational a

--------------------------------------------------------------------------------
fTruncate, fRound, fCeiling, fFloor :: Field -> Field

fTruncate 	(FDouble (Just a)) 	= FInt $ Just $ truncate a
fTruncate 	a			= error $ "fTruncate: " ++ show a

fRound 		(FDouble (Just a)) 	= FInt $ Just $ round a
fRound	 	a			= error $ "fRound: " ++ show a

fCeiling 	(FDouble (Just a)) 	= FInt $ Just $ ceiling a
fCeiling 	a		 	= error $ "fCeiling: " ++ show a

fFloor	 	(FDouble (Just a)) 	= FInt $ Just $ floor a
fFloor	 	a		 	= error $ "fFloor: " ++ show a
