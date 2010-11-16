{-# OPTIONS_GHC -fglasgow-exts #-}
module RMCProtobuf.RMC.RMCTime (RMCTime(..)) where
import Prelude ((+))
import qualified Prelude as P'
import qualified Text.ProtocolBuffers.Header as P'
 
data RMCTime = RMCTime{rmcHours :: P'.Int32, rmcMinutes :: P'.Int32, rmcSeconds :: P'.Double}
             deriving (P'.Show, P'.Eq, P'.Ord, P'.Typeable)
 
instance P'.Mergeable RMCTime where
  mergeEmpty = RMCTime P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty
  mergeAppend (RMCTime x'1 x'2 x'3) (RMCTime y'1 y'2 y'3)
   = RMCTime (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3)
 
instance P'.Default RMCTime where
  defaultValue = RMCTime P'.defaultValue P'.defaultValue P'.defaultValue
 
instance P'.Wire RMCTime where
  wireSize ft' self'@(RMCTime x'1 x'2 x'3)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeReq 1 5 x'1 + P'.wireSizeReq 1 5 x'2 + P'.wireSizeReq 1 1 x'3)
  wirePut ft' self'@(RMCTime x'1 x'2 x'3)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutReq 8 5 x'1
             P'.wirePutReq 16 5 x'2
             P'.wirePutReq 25 1 x'3
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             8 -> P'.fmap (\ new'Field -> old'Self{rmcHours = new'Field}) (P'.wireGet 5)
             16 -> P'.fmap (\ new'Field -> old'Self{rmcMinutes = new'Field}) (P'.wireGet 5)
             25 -> P'.fmap (\ new'Field -> old'Self{rmcSeconds = new'Field}) (P'.wireGet 1)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> RMCTime) RMCTime where
  getVal m' f' = f' m'
 
instance P'.GPB RMCTime
 
instance P'.ReflectDescriptor RMCTime where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList [8, 16, 25]) (P'.fromDistinctAscList [8, 16, 25])
  reflectDescriptorInfo _
   = P'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".RMC.RMCTime\", haskellPrefix = [MName \"RMCProtobuf\"], parentModule = [MName \"RMC\"], baseName = MName \"RMCTime\"}, descFilePath = [\"RMCProtobuf\",\"RMC\",\"RMCTime.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".RMC.RMCTime.rmcHours\", haskellPrefix' = [MName \"RMCProtobuf\"], parentModule' = [MName \"RMC\",MName \"RMCTime\"], baseName' = FName \"rmcHours\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 8}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".RMC.RMCTime.rmcMinutes\", haskellPrefix' = [MName \"RMCProtobuf\"], parentModule' = [MName \"RMC\",MName \"RMCTime\"], baseName' = FName \"rmcMinutes\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 16}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".RMC.RMCTime.rmcSeconds\", haskellPrefix' = [MName \"RMCProtobuf\"], parentModule' = [MName \"RMC\",MName \"RMCTime\"], baseName' = FName \"rmcSeconds\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 25}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 1}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False}"