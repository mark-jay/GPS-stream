module SqlStream.Fields(Field(..), fTruncate, fRound, fCeiling, fFloor, Row, TableMeta) where

-- FIXME instances
import Data.Time
import Data.Int
import Data.Ratio
import Control.Monad
import Control.Applicative((<|>))
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
    deriving(Show, Read, Eq, Ord)

type Row = [Field]

-- FIXME 
type TableMeta = ()

mbOp :: (a -> a -> a) -> (Maybe a -> Maybe a -> Maybe a)
mbOp fn (Just a) (Just b) = Just (a `fn` b)
mbOp fn a	 b	  = a <|> b

mbPlus :: (Num a) => Maybe a -> Maybe a -> Maybe a
mbPlus  = mbOp (+)

mbMult :: (Num a) => Maybe a -> Maybe a -> Maybe a
mbMult  = mbOp (*)

mbFracDiv :: (Fractional a) => Maybe a -> Maybe a -> Maybe a
mbFracDiv = mbOp (/)

fromIntegralMb :: (Integral a, Num b) => Maybe a -> Maybe b
fromIntegralMb  = liftM fromIntegral

notImplemented = error . (++ " is not implemented yet")

instance Num Field where
  -- plus
  (FDouble a) + (FDouble b) = FDouble $ a `mbPlus` b
  (FInt a)    + (FInt b)    = FInt    $ a `mbPlus` b
  (FInt32 a)  + (FInt32 b)  = FInt32  $ a `mbPlus` b

  (FInt a)    + (FDouble b) = FDouble $ fromIntegralMb a `mbPlus` b
  (FDouble a) + (FInt b)    = FDouble $ a `mbPlus` fromIntegralMb b

  -- mult
  (FDouble a) * (FDouble b) = FDouble $ a `mbMult` b
  (FInt a)    * (FInt b)    = FInt    $ a `mbMult` b
  (FInt32 a)  * (FInt32 b)  = FInt32  $ a `mbMult` b

  (FInt a)    * (FDouble b) = FDouble $ fromIntegralMb a `mbMult` b
  (FDouble a) * (FInt b)    = FDouble $ a `mbMult` fromIntegralMb b

  -- fromInteger
  fromInteger	= FInt . Just . fromInteger

  (-)		= notImplemented "-"
  negate	= notImplemented "negate"
  abs		= notImplemented "abs"
  signum	= notImplemented "signum"

--------------------------------------------------------------------------------

{-
-- if diferrent type comparing is neccessary
instance Ord Field where
    compare (FInt a)     (FInt b)	= compare a b
    compare (FInt32 a)   (FInt32 b) 	= compare a b
    compare (FDouble a)  (FDouble b) 	= compare a b
    compare (FDayTime a) (FDayTime b) 	= compare a b

    compare (FInt a) 	 (FDouble b)	= compare (fromIntegralMb a) b
    compare (FDouble a)	 (FInt b)	= compare a (fromIntegralMb b)

    compare (FRMCStatus a)(FRMCStatus b)= compare a b
    compare (FMDeclDir a) (FMDeclDir b) = compare a b
    compare (FModeInd a)  (FModeInd b)  = compare a b

-}

--------------------------------------------------------------------------------
instance Fractional Field where
    FDouble a	/ FDouble b		= FDouble (a `mbFracDiv` b)

    recip (FDouble a)			= FDouble (liftM recip a)

    fromRational 			= FDouble . Just . fromRational

--------------------------------------------------------------------------------
instance Real Field where
    toRational (FDouble (Just a))	= toRational a
    toRational (FInt    (Just a))	= toRational a
    toRational (FInt32  (Just a))	= toRational a

--------------------------------------------------------------------------------
fTruncate, fRound, fCeiling, fFloor :: Field -> Field

fTruncate 	(FDouble (Just a)) 	= FInt $ Just $ truncate a

fRound 		(FDouble (Just a)) 	= FInt $ Just $ round a

fCeiling 	(FDouble (Just a)) 	= FInt $ Just $ ceiling a

fFloor	 	(FDouble (Just a)) 	= FInt $ Just $ floor a
