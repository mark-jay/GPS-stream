{-# OPTIONS_GHC -fglasgow-exts #-}
module RMC.Protobuf.RMC.RMC.Status (Status(..)) where
import Prelude ((+), (.))
import qualified Prelude as P'
import qualified Text.ProtocolBuffers.Header as P'
 
data Status = A
            | V
            deriving (P'.Read, P'.Show, P'.Eq, P'.Ord, P'.Typeable)
 
instance P'.Mergeable Status
 
instance P'.Bounded Status where
  minBound = A
  maxBound = V
 
instance P'.Default Status where
  defaultValue = A
 
toMaybe'Enum :: P'.Int -> P'.Maybe Status
toMaybe'Enum 0 = P'.Just A
toMaybe'Enum 1 = P'.Just V
toMaybe'Enum _ = P'.Nothing
 
instance P'.Enum Status where
  fromEnum A = 0
  fromEnum V = 1
  toEnum = P'.fromMaybe (P'.error "hprotoc generated code: toEnum failure for type RMC.Protobuf.RMC.RMC.Status") . toMaybe'Enum
  succ A = V
  succ _ = P'.error "hprotoc generated code: succ failure for type RMC.Protobuf.RMC.RMC.Status"
  pred V = A
  pred _ = P'.error "hprotoc generated code: pred failure for type RMC.Protobuf.RMC.RMC.Status"
 
instance P'.Wire Status where
  wireSize ft' enum = P'.wireSize ft' (P'.fromEnum enum)
  wirePut ft' enum = P'.wirePut ft' (P'.fromEnum enum)
  wireGet 14 = P'.wireGetEnum toMaybe'Enum
  wireGet ft' = P'.wireGetErr ft'
  wireGetPacked 14 = P'.wireGetPackedEnum toMaybe'Enum
  wireGetPacked ft' = P'.wireGetErr ft'
 
instance P'.GPB Status
 
instance P'.MessageAPI msg' (msg' -> Status) Status where
  getVal m' f' = f' m'
 
instance P'.ReflectEnum Status where
  reflectEnum = [(0, "A", A), (1, "V", V)]
  reflectEnumInfo _
   = P'.EnumInfo (P'.makePNF (P'.pack ".RMC.RMC.Status") ["RMC", "Protobuf"] ["RMC", "RMC"] "Status")
      ["RMC", "Protobuf", "RMC", "RMC", "Status.hs"]
      [(0, "A"), (1, "V")]