{-# OPTIONS_GHC -fglasgow-exts #-}
module RMCProtobuf.RMC.RMCDate (RMCDate(..)) where
import Prelude ((+))
import qualified Prelude as P'
import qualified Text.ProtocolBuffers.Header as P'
 
data RMCDate = RMCDate{day :: P'.Int32, month :: P'.Int32, year :: P'.Int32}
             deriving (P'.Show, P'.Eq, P'.Ord, P'.Typeable)
 
instance P'.Mergeable RMCDate where
  mergeEmpty = RMCDate P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty
  mergeAppend (RMCDate x'1 x'2 x'3) (RMCDate y'1 y'2 y'3)
   = RMCDate (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3)
 
instance P'.Default RMCDate where
  defaultValue = RMCDate P'.defaultValue P'.defaultValue P'.defaultValue
 
instance P'.Wire RMCDate where
  wireSize ft' self'@(RMCDate x'1 x'2 x'3)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeReq 1 5 x'1 + P'.wireSizeReq 1 5 x'2 + P'.wireSizeReq 1 5 x'3)
  wirePut ft' self'@(RMCDate x'1 x'2 x'3)
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
             P'.wirePutReq 24 5 x'3
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             8 -> P'.fmap (\ new'Field -> old'Self{day = new'Field}) (P'.wireGet 5)
             16 -> P'.fmap (\ new'Field -> old'Self{month = new'Field}) (P'.wireGet 5)
             24 -> P'.fmap (\ new'Field -> old'Self{year = new'Field}) (P'.wireGet 5)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> RMCDate) RMCDate where
  getVal m' f' = f' m'
 
instance P'.GPB RMCDate
 
instance P'.ReflectDescriptor RMCDate where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList [8, 16, 24]) (P'.fromDistinctAscList [8, 16, 24])
  reflectDescriptorInfo _
   = P'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".RMC.RMCDate\", haskellPrefix = [MName \"RMCProtobuf\"], parentModule = [MName \"RMC\"], baseName = MName \"RMCDate\"}, descFilePath = [\"RMCProtobuf\",\"RMC\",\"RMCDate.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".RMC.RMCDate.day\", haskellPrefix' = [MName \"RMCProtobuf\"], parentModule' = [MName \"RMC\",MName \"RMCDate\"], baseName' = FName \"day\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 8}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".RMC.RMCDate.month\", haskellPrefix' = [MName \"RMCProtobuf\"], parentModule' = [MName \"RMC\",MName \"RMCDate\"], baseName' = FName \"month\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 16}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".RMC.RMCDate.year\", haskellPrefix' = [MName \"RMCProtobuf\"], parentModule' = [MName \"RMC\",MName \"RMCDate\"], baseName' = FName \"year\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 24}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False}"