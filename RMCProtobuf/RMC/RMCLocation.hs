{-# OPTIONS_GHC -fglasgow-exts #-}
module RMCProtobuf.RMC.RMCLocation (RMCLocation(..)) where
import Prelude ((+))
import qualified Prelude as P'
import qualified Text.ProtocolBuffers.Header as P'
import qualified RMCProtobuf.RMC.Angle as RMC (Angle)
 
data RMCLocation = RMCLocation{latitude :: P'.Maybe RMC.Angle, p :: P'.Maybe P'.Utf8, longitude :: P'.Maybe RMC.Angle,
                               j :: P'.Maybe P'.Utf8}
                 deriving (P'.Show, P'.Eq, P'.Ord, P'.Typeable)
 
instance P'.Mergeable RMCLocation where
  mergeEmpty = RMCLocation P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty
  mergeAppend (RMCLocation x'1 x'2 x'3 x'4) (RMCLocation y'1 y'2 y'3 y'4)
   = RMCLocation (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)
 
instance P'.Default RMCLocation where
  defaultValue = RMCLocation P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue
 
instance P'.Wire RMCLocation where
  wireSize ft' self'@(RMCLocation x'1 x'2 x'3 x'4)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeOpt 1 11 x'1 + P'.wireSizeOpt 1 9 x'2 + P'.wireSizeOpt 1 11 x'3 + P'.wireSizeOpt 1 9 x'4)
  wirePut ft' self'@(RMCLocation x'1 x'2 x'3 x'4)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutOpt 10 11 x'1
             P'.wirePutOpt 18 9 x'2
             P'.wirePutOpt 26 11 x'3
             P'.wirePutOpt 34 9 x'4
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             10 -> P'.fmap (\ new'Field -> old'Self{latitude = P'.mergeAppend (latitude old'Self) (P'.Just new'Field)})
                    (P'.wireGet 11)
             18 -> P'.fmap (\ new'Field -> old'Self{p = P'.Just new'Field}) (P'.wireGet 9)
             26 -> P'.fmap (\ new'Field -> old'Self{longitude = P'.mergeAppend (longitude old'Self) (P'.Just new'Field)})
                    (P'.wireGet 11)
             34 -> P'.fmap (\ new'Field -> old'Self{j = P'.Just new'Field}) (P'.wireGet 9)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> RMCLocation) RMCLocation where
  getVal m' f' = f' m'
 
instance P'.GPB RMCLocation
 
instance P'.ReflectDescriptor RMCLocation where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [10, 18, 26, 34])
  reflectDescriptorInfo _
   = P'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".RMC.RMCLocation\", haskellPrefix = [MName \"RMCProtobuf\"], parentModule = [MName \"RMC\"], baseName = MName \"RMCLocation\"}, descFilePath = [\"RMCProtobuf\",\"RMC\",\"RMCLocation.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".RMC.RMCLocation.latitude\", haskellPrefix' = [MName \"RMCProtobuf\"], parentModule' = [MName \"RMC\",MName \"RMCLocation\"], baseName' = FName \"latitude\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".RMC.Angle\", haskellPrefix = [MName \"RMCProtobuf\"], parentModule = [MName \"RMC\"], baseName = MName \"Angle\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".RMC.RMCLocation.p\", haskellPrefix' = [MName \"RMCProtobuf\"], parentModule' = [MName \"RMC\",MName \"RMCLocation\"], baseName' = FName \"p\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".RMC.RMCLocation.longitude\", haskellPrefix' = [MName \"RMCProtobuf\"], parentModule' = [MName \"RMC\",MName \"RMCLocation\"], baseName' = FName \"longitude\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 26}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".RMC.Angle\", haskellPrefix = [MName \"RMCProtobuf\"], parentModule = [MName \"RMC\"], baseName = MName \"Angle\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".RMC.RMCLocation.j\", haskellPrefix' = [MName \"RMCProtobuf\"], parentModule' = [MName \"RMC\",MName \"RMCLocation\"], baseName' = FName \"j\"}, fieldNumber = FieldId {getFieldId = 4}, wireTag = WireTag {getWireTag = 34}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False}"