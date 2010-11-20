{-# OPTIONS_GHC -fglasgow-exts #-}
module RMC.Protobuf.RMC.RMC.MDeclDir (MDeclDir(..)) where
import Prelude ((+), (.))
import qualified Prelude as P'
import qualified Text.ProtocolBuffers.Header as P'
 
data MDeclDir = E
              | W
              deriving (P'.Read, P'.Show, P'.Eq, P'.Ord, P'.Typeable)
 
instance P'.Mergeable MDeclDir
 
instance P'.Bounded MDeclDir where
  minBound = E
  maxBound = W
 
instance P'.Default MDeclDir where
  defaultValue = E
 
toMaybe'Enum :: P'.Int -> P'.Maybe MDeclDir
toMaybe'Enum 0 = P'.Just E
toMaybe'Enum 1 = P'.Just W
toMaybe'Enum _ = P'.Nothing
 
instance P'.Enum MDeclDir where
  fromEnum E = 0
  fromEnum W = 1
  toEnum = P'.fromMaybe (P'.error "hprotoc generated code: toEnum failure for type RMC.Protobuf.RMC.RMC.MDeclDir") . toMaybe'Enum
  succ E = W
  succ _ = P'.error "hprotoc generated code: succ failure for type RMC.Protobuf.RMC.RMC.MDeclDir"
  pred W = E
  pred _ = P'.error "hprotoc generated code: pred failure for type RMC.Protobuf.RMC.RMC.MDeclDir"
 
instance P'.Wire MDeclDir where
  wireSize ft' enum = P'.wireSize ft' (P'.fromEnum enum)
  wirePut ft' enum = P'.wirePut ft' (P'.fromEnum enum)
  wireGet 14 = P'.wireGetEnum toMaybe'Enum
  wireGet ft' = P'.wireGetErr ft'
  wireGetPacked 14 = P'.wireGetPackedEnum toMaybe'Enum
  wireGetPacked ft' = P'.wireGetErr ft'
 
instance P'.GPB MDeclDir
 
instance P'.MessageAPI msg' (msg' -> MDeclDir) MDeclDir where
  getVal m' f' = f' m'
 
instance P'.ReflectEnum MDeclDir where
  reflectEnum = [(0, "E", E), (1, "W", W)]
  reflectEnumInfo _
   = P'.EnumInfo (P'.makePNF (P'.pack ".RMC.RMC.MDeclDir") ["RMC", "Protobuf"] ["RMC", "RMC"] "MDeclDir")
      ["RMC", "Protobuf", "RMC", "RMC", "MDeclDir.hs"]
      [(0, "E"), (1, "W")]