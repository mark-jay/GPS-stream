{-# OPTIONS_GHC -fglasgow-exts #-}
module RMCProtobuf.RMC.RMC (RMC(..)) where
import Prelude ((+))
import qualified Prelude as P'
import qualified Text.ProtocolBuffers.Header as P'
import qualified RMCProtobuf.RMC.RMC.MDeclDir as RMC.RMC (MDeclDir)
import qualified RMCProtobuf.RMC.RMC.ModeInd as RMC.RMC (ModeInd)
import qualified RMCProtobuf.RMC.RMC.Status as RMC.RMC (Status)
 
data RMC = RMC{time :: P'.Maybe P'.Double, status :: P'.Maybe RMC.RMC.Status, latitude :: P'.Maybe P'.Double,
               longitude :: P'.Maybe P'.Double, speed :: P'.Maybe P'.Double, direction :: P'.Maybe P'.Double,
               date :: P'.Maybe P'.Int32, mDecl :: P'.Maybe P'.Double, mDeclDir :: P'.Maybe RMC.RMC.MDeclDir,
               modeInd :: P'.Maybe RMC.RMC.ModeInd}
         deriving (P'.Show, P'.Eq, P'.Ord, P'.Typeable)
 
instance P'.Mergeable RMC where
  mergeEmpty
   = RMC P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty P'.mergeEmpty
      P'.mergeEmpty
      P'.mergeEmpty
  mergeAppend (RMC x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9 x'10) (RMC y'1 y'2 y'3 y'4 y'5 y'6 y'7 y'8 y'9 y'10)
   = RMC (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)
      (P'.mergeAppend x'5 y'5)
      (P'.mergeAppend x'6 y'6)
      (P'.mergeAppend x'7 y'7)
      (P'.mergeAppend x'8 y'8)
      (P'.mergeAppend x'9 y'9)
      (P'.mergeAppend x'10 y'10)
 
instance P'.Default RMC where
  defaultValue
   = RMC P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue
      P'.defaultValue
      P'.defaultValue
      P'.defaultValue
 
instance P'.Wire RMC where
  wireSize ft' self'@(RMC x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9 x'10)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size
         = (P'.wireSizeOpt 1 1 x'1 + P'.wireSizeOpt 1 14 x'2 + P'.wireSizeOpt 1 1 x'3 + P'.wireSizeOpt 1 1 x'4 +
             P'.wireSizeOpt 1 1 x'5
             + P'.wireSizeOpt 1 1 x'6
             + P'.wireSizeOpt 1 5 x'7
             + P'.wireSizeOpt 1 1 x'8
             + P'.wireSizeOpt 1 14 x'9
             + P'.wireSizeOpt 1 14 x'10)
  wirePut ft' self'@(RMC x'1 x'2 x'3 x'4 x'5 x'6 x'7 x'8 x'9 x'10)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutOpt 9 1 x'1
             P'.wirePutOpt 16 14 x'2
             P'.wirePutOpt 25 1 x'3
             P'.wirePutOpt 33 1 x'4
             P'.wirePutOpt 41 1 x'5
             P'.wirePutOpt 49 1 x'6
             P'.wirePutOpt 56 5 x'7
             P'.wirePutOpt 65 1 x'8
             P'.wirePutOpt 72 14 x'9
             P'.wirePutOpt 80 14 x'10
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             9 -> P'.fmap (\ new'Field -> old'Self{time = P'.Just new'Field}) (P'.wireGet 1)
             16 -> P'.fmap (\ new'Field -> old'Self{status = P'.Just new'Field}) (P'.wireGet 14)
             25 -> P'.fmap (\ new'Field -> old'Self{latitude = P'.Just new'Field}) (P'.wireGet 1)
             33 -> P'.fmap (\ new'Field -> old'Self{longitude = P'.Just new'Field}) (P'.wireGet 1)
             41 -> P'.fmap (\ new'Field -> old'Self{speed = P'.Just new'Field}) (P'.wireGet 1)
             49 -> P'.fmap (\ new'Field -> old'Self{direction = P'.Just new'Field}) (P'.wireGet 1)
             56 -> P'.fmap (\ new'Field -> old'Self{date = P'.Just new'Field}) (P'.wireGet 5)
             65 -> P'.fmap (\ new'Field -> old'Self{mDecl = P'.Just new'Field}) (P'.wireGet 1)
             72 -> P'.fmap (\ new'Field -> old'Self{mDeclDir = P'.Just new'Field}) (P'.wireGet 14)
             80 -> P'.fmap (\ new'Field -> old'Self{modeInd = P'.Just new'Field}) (P'.wireGet 14)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> RMC) RMC where
  getVal m' f' = f' m'
 
instance P'.GPB RMC
 
instance P'.ReflectDescriptor RMC where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [9, 16, 25, 33, 41, 49, 56, 65, 72, 80])
  reflectDescriptorInfo _
   = P'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".RMC.RMC\", haskellPrefix = [MName \"RMCProtobuf\"], parentModule = [MName \"RMC\"], baseName = MName \"RMC\"}, descFilePath = [\"RMCProtobuf\",\"RMC\",\"RMC.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".RMC.RMC.time\", haskellPrefix' = [MName \"RMCProtobuf\"], parentModule' = [MName \"RMC\",MName \"RMC\"], baseName' = FName \"time\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 9}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 1}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".RMC.RMC.status\", haskellPrefix' = [MName \"RMCProtobuf\"], parentModule' = [MName \"RMC\",MName \"RMC\"], baseName' = FName \"status\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 16}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 14}, typeName = Just (ProtoName {protobufName = FIName \".RMC.RMC.Status\", haskellPrefix = [MName \"RMCProtobuf\"], parentModule = [MName \"RMC\",MName \"RMC\"], baseName = MName \"Status\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".RMC.RMC.latitude\", haskellPrefix' = [MName \"RMCProtobuf\"], parentModule' = [MName \"RMC\",MName \"RMC\"], baseName' = FName \"latitude\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 25}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 1}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".RMC.RMC.longitude\", haskellPrefix' = [MName \"RMCProtobuf\"], parentModule' = [MName \"RMC\",MName \"RMC\"], baseName' = FName \"longitude\"}, fieldNumber = FieldId {getFieldId = 4}, wireTag = WireTag {getWireTag = 33}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 1}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".RMC.RMC.speed\", haskellPrefix' = [MName \"RMCProtobuf\"], parentModule' = [MName \"RMC\",MName \"RMC\"], baseName' = FName \"speed\"}, fieldNumber = FieldId {getFieldId = 5}, wireTag = WireTag {getWireTag = 41}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 1}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".RMC.RMC.direction\", haskellPrefix' = [MName \"RMCProtobuf\"], parentModule' = [MName \"RMC\",MName \"RMC\"], baseName' = FName \"direction\"}, fieldNumber = FieldId {getFieldId = 6}, wireTag = WireTag {getWireTag = 49}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 1}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".RMC.RMC.date\", haskellPrefix' = [MName \"RMCProtobuf\"], parentModule' = [MName \"RMC\",MName \"RMC\"], baseName' = FName \"date\"}, fieldNumber = FieldId {getFieldId = 7}, wireTag = WireTag {getWireTag = 56}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".RMC.RMC.mDecl\", haskellPrefix' = [MName \"RMCProtobuf\"], parentModule' = [MName \"RMC\",MName \"RMC\"], baseName' = FName \"mDecl\"}, fieldNumber = FieldId {getFieldId = 8}, wireTag = WireTag {getWireTag = 65}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 1}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".RMC.RMC.mDeclDir\", haskellPrefix' = [MName \"RMCProtobuf\"], parentModule' = [MName \"RMC\",MName \"RMC\"], baseName' = FName \"mDeclDir\"}, fieldNumber = FieldId {getFieldId = 9}, wireTag = WireTag {getWireTag = 72}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 14}, typeName = Just (ProtoName {protobufName = FIName \".RMC.RMC.MDeclDir\", haskellPrefix = [MName \"RMCProtobuf\"], parentModule = [MName \"RMC\",MName \"RMC\"], baseName = MName \"MDeclDir\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".RMC.RMC.modeInd\", haskellPrefix' = [MName \"RMCProtobuf\"], parentModule' = [MName \"RMC\",MName \"RMC\"], baseName' = FName \"modeInd\"}, fieldNumber = FieldId {getFieldId = 10}, wireTag = WireTag {getWireTag = 80}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 14}, typeName = Just (ProtoName {protobufName = FIName \".RMC.RMC.ModeInd\", haskellPrefix = [MName \"RMCProtobuf\"], parentModule = [MName \"RMC\",MName \"RMC\"], baseName = MName \"ModeInd\"}), hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False}"