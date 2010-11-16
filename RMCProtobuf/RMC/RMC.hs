{-# OPTIONS_GHC -fglasgow-exts #-}
module RMCProtobuf.RMC.RMC (RMC(..)) where
import Prelude ((+))
import qualified Prelude as P'
import qualified Text.ProtocolBuffers.Header as P'
import qualified RMCProtobuf.RMC.RMCDate as RMC (RMCDate)
import qualified RMCProtobuf.RMC.RMCLocation as RMC (RMCLocation)
import qualified RMCProtobuf.RMC.RMCTime as RMC (RMCTime)
 
data RMC = RMC{time :: P'.Maybe RMC.RMCTime, status :: P'.Maybe P'.Utf8, location :: RMC.RMCLocation, speed :: P'.Maybe P'.Double,
               sDirection :: P'.Maybe P'.Double, date :: P'.Maybe RMC.RMCDate, mDecl :: P'.Maybe P'.Double,
               mDeclDir :: P'.Maybe P'.Utf8, modeIndicator :: P'.Maybe P'.Utf8}
         deriving (P'.Show, P'.Eq, P'.Ord, P'.Typeable)
 
instance P'.Mergeable RMC where
  mergeEmpty
   = RMC P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty
      P'.mergeEmpty
  mergeAppend (RMC x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9) (RMC y'1 y'2 y'3 y'4 y'5 y'6 y'7 y'8 y'9)
   = RMC (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)
      (P'.mergeAppend x'5 y'5)
      (P'.mergeAppend x'6 y'6)
      (P'.mergeAppend x'7 y'7)
      (P'.mergeAppend x'8 y'8)
      (P'.mergeAppend x'9 y'9)
 
instance P'.Default RMC where
  defaultValue
   = RMC P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue
      P'.defaultValue
      P'.defaultValue
 
instance P'.Wire RMC where
  wireSize ft' self'@(RMC x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size
         = (P'.wireSizeOpt 1 11 x'1 + P'.wireSizeOpt 1 9 x'2 + P'.wireSizeReq 1 11 x'3 + P'.wireSizeOpt 1 1 x'4 +
             P'.wireSizeOpt 1 1 x'5
             + P'.wireSizeOpt 1 11 x'6
             + P'.wireSizeOpt 1 1 x'7
             + P'.wireSizeOpt 1 9 x'8
             + P'.wireSizeOpt 1 9 x'9)
  wirePut ft' self'@(RMC x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9)
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
             P'.wirePutReq 26 11 x'3
             P'.wirePutOpt 33 1 x'4
             P'.wirePutOpt 41 1 x'5
             P'.wirePutOpt 50 11 x'6
             P'.wirePutOpt 57 1 x'7
             P'.wirePutOpt 66 9 x'8
             P'.wirePutOpt 74 9 x'9
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             10 -> P'.fmap (\ new'Field -> old'Self{time = P'.mergeAppend (time old'Self) (P'.Just new'Field)}) (P'.wireGet 11)
             18 -> P'.fmap (\ new'Field -> old'Self{status = P'.Just new'Field}) (P'.wireGet 9)
             26 -> P'.fmap (\ new'Field -> old'Self{location = P'.mergeAppend (location old'Self) (new'Field)}) (P'.wireGet 11)
             33 -> P'.fmap (\ new'Field -> old'Self{speed = P'.Just new'Field}) (P'.wireGet 1)
             41 -> P'.fmap (\ new'Field -> old'Self{sDirection = P'.Just new'Field}) (P'.wireGet 1)
             50 -> P'.fmap (\ new'Field -> old'Self{date = P'.mergeAppend (date old'Self) (P'.Just new'Field)}) (P'.wireGet 11)
             57 -> P'.fmap (\ new'Field -> old'Self{mDecl = P'.Just new'Field}) (P'.wireGet 1)
             66 -> P'.fmap (\ new'Field -> old'Self{mDeclDir = P'.Just new'Field}) (P'.wireGet 9)
             74 -> P'.fmap (\ new'Field -> old'Self{modeIndicator = P'.Just new'Field}) (P'.wireGet 9)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> RMC) RMC where
  getVal m' f' = f' m'
 
instance P'.GPB RMC
 
instance P'.ReflectDescriptor RMC where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList [26]) (P'.fromDistinctAscList [10, 18, 26, 33, 41, 50, 57, 66, 74])
  reflectDescriptorInfo _
   = P'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".RMC.RMC\", haskellPrefix = [MName \"RMCProtobuf\"], parentModule = [MName \"RMC\"], baseName = MName \"RMC\"}, descFilePath = [\"RMCProtobuf\",\"RMC\",\"RMC.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".RMC.RMC.time\", haskellPrefix' = [MName \"RMCProtobuf\"], parentModule' = [MName \"RMC\",MName \"RMC\"], baseName' = FName \"time\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".RMC.RMCTime\", haskellPrefix = [MName \"RMCProtobuf\"], parentModule = [MName \"RMC\"], baseName = MName \"RMCTime\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".RMC.RMC.status\", haskellPrefix' = [MName \"RMCProtobuf\"], parentModule' = [MName \"RMC\",MName \"RMC\"], baseName' = FName \"status\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".RMC.RMC.location\", haskellPrefix' = [MName \"RMCProtobuf\"], parentModule' = [MName \"RMC\",MName \"RMC\"], baseName' = FName \"location\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 26}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".RMC.RMCLocation\", haskellPrefix = [MName \"RMCProtobuf\"], parentModule = [MName \"RMC\"], baseName = MName \"RMCLocation\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".RMC.RMC.speed\", haskellPrefix' = [MName \"RMCProtobuf\"], parentModule' = [MName \"RMC\",MName \"RMC\"], baseName' = FName \"speed\"}, fieldNumber = FieldId {getFieldId = 4}, wireTag = WireTag {getWireTag = 33}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 1}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".RMC.RMC.sDirection\", haskellPrefix' = [MName \"RMCProtobuf\"], parentModule' = [MName \"RMC\",MName \"RMC\"], baseName' = FName \"sDirection\"}, fieldNumber = FieldId {getFieldId = 5}, wireTag = WireTag {getWireTag = 41}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 1}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".RMC.RMC.date\", haskellPrefix' = [MName \"RMCProtobuf\"], parentModule' = [MName \"RMC\",MName \"RMC\"], baseName' = FName \"date\"}, fieldNumber = FieldId {getFieldId = 6}, wireTag = WireTag {getWireTag = 50}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".RMC.RMCDate\", haskellPrefix = [MName \"RMCProtobuf\"], parentModule = [MName \"RMC\"], baseName = MName \"RMCDate\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".RMC.RMC.mDecl\", haskellPrefix' = [MName \"RMCProtobuf\"], parentModule' = [MName \"RMC\",MName \"RMC\"], baseName' = FName \"mDecl\"}, fieldNumber = FieldId {getFieldId = 7}, wireTag = WireTag {getWireTag = 57}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 1}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".RMC.RMC.mDeclDir\", haskellPrefix' = [MName \"RMCProtobuf\"], parentModule' = [MName \"RMC\",MName \"RMC\"], baseName' = FName \"mDeclDir\"}, fieldNumber = FieldId {getFieldId = 8}, wireTag = WireTag {getWireTag = 66}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".RMC.RMC.modeIndicator\", haskellPrefix' = [MName \"RMCProtobuf\"], parentModule' = [MName \"RMC\",MName \"RMC\"], baseName' = FName \"modeIndicator\"}, fieldNumber = FieldId {getFieldId = 9}, wireTag = WireTag {getWireTag = 74}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False}"