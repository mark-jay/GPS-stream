{-# OPTIONS_GHC -fglasgow-exts #-}
module RMCProtobuf.RMC.Angle (Angle(..)) where
import Prelude ((+))
import qualified Prelude as P'
import qualified Text.ProtocolBuffers.Header as P'
 
data Angle = Angle{degree :: P'.Int32, minute :: P'.Double}
           deriving (P'.Show, P'.Eq, P'.Ord, P'.Typeable)
 
instance P'.Mergeable Angle where
  mergeEmpty = Angle P'.mergeEmpty P'.mergeEmpty
  mergeAppend (Angle x'1 x'2) (Angle y'1 y'2) = Angle (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2)
 
instance P'.Default Angle where
  defaultValue = Angle P'.defaultValue P'.defaultValue
 
instance P'.Wire Angle where
  wireSize ft' self'@(Angle x'1 x'2)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeReq 1 5 x'1 + P'.wireSizeReq 1 1 x'2)
  wirePut ft' self'@(Angle x'1 x'2)
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
             P'.wirePutReq 17 1 x'2
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             8 -> P'.fmap (\ new'Field -> old'Self{degree = new'Field}) (P'.wireGet 5)
             17 -> P'.fmap (\ new'Field -> old'Self{minute = new'Field}) (P'.wireGet 1)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> Angle) Angle where
  getVal m' f' = f' m'
 
instance P'.GPB Angle
 
instance P'.ReflectDescriptor Angle where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList [8, 17]) (P'.fromDistinctAscList [8, 17])
  reflectDescriptorInfo _
   = P'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".RMC.Angle\", haskellPrefix = [MName \"RMCProtobuf\"], parentModule = [MName \"RMC\"], baseName = MName \"Angle\"}, descFilePath = [\"RMCProtobuf\",\"RMC\",\"Angle.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".RMC.Angle.degree\", haskellPrefix' = [MName \"RMCProtobuf\"], parentModule' = [MName \"RMC\",MName \"Angle\"], baseName' = FName \"degree\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 8}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".RMC.Angle.minute\", haskellPrefix' = [MName \"RMCProtobuf\"], parentModule' = [MName \"RMC\",MName \"Angle\"], baseName' = FName \"minute\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 17}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 1}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False}"