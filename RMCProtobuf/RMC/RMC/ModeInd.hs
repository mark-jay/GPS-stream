{-# OPTIONS_GHC -fglasgow-exts #-}
module RMCProtobuf.RMC.RMC.ModeInd (ModeInd(..)) where
import Prelude ((+), (.))
import qualified Prelude as P'
import qualified Text.ProtocolBuffers.Header as P'
 
data ModeInd = A
             | D
             | E
             | N
             deriving (P'.Read, P'.Show, P'.Eq, P'.Ord, P'.Typeable)
 
instance P'.Mergeable ModeInd
 
instance P'.Bounded ModeInd where
  minBound = A
  maxBound = N
 
instance P'.Default ModeInd where
  defaultValue = A
 
toMaybe'Enum :: P'.Int -> P'.Maybe ModeInd
toMaybe'Enum 0 = P'.Just A
toMaybe'Enum 1 = P'.Just D
toMaybe'Enum 2 = P'.Just E
toMaybe'Enum 3 = P'.Just N
toMaybe'Enum _ = P'.Nothing
 
instance P'.Enum ModeInd where
  fromEnum A = 0
  fromEnum D = 1
  fromEnum E = 2
  fromEnum N = 3
  toEnum = P'.fromMaybe (P'.error "hprotoc generated code: toEnum failure for type RMCProtobuf.RMC.RMC.ModeInd") . toMaybe'Enum
  succ A = D
  succ D = E
  succ E = N
  succ _ = P'.error "hprotoc generated code: succ failure for type RMCProtobuf.RMC.RMC.ModeInd"
  pred D = A
  pred E = D
  pred N = E
  pred _ = P'.error "hprotoc generated code: pred failure for type RMCProtobuf.RMC.RMC.ModeInd"
 
instance P'.Wire ModeInd where
  wireSize ft' enum = P'.wireSize ft' (P'.fromEnum enum)
  wirePut ft' enum = P'.wirePut ft' (P'.fromEnum enum)
  wireGet 14 = P'.wireGetEnum toMaybe'Enum
  wireGet ft' = P'.wireGetErr ft'
  wireGetPacked 14 = P'.wireGetPackedEnum toMaybe'Enum
  wireGetPacked ft' = P'.wireGetErr ft'
 
instance P'.GPB ModeInd
 
instance P'.MessageAPI msg' (msg' -> ModeInd) ModeInd where
  getVal m' f' = f' m'
 
instance P'.ReflectEnum ModeInd where
  reflectEnum = [(0, "A", A), (1, "D", D), (2, "E", E), (3, "N", N)]
  reflectEnumInfo _
   = P'.EnumInfo (P'.makePNF (P'.pack ".RMC.RMC.ModeInd") ["RMCProtobuf"] ["RMC", "RMC"] "ModeInd")
      ["RMCProtobuf", "RMC", "RMC", "ModeInd.hs"]
      [(0, "A"), (1, "D"), (2, "E"), (3, "N")]