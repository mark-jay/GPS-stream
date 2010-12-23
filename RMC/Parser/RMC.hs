module RMC.Parser.RMC where

import RMC.Protobuf.RMC.RMC
import RMC.API
import Doc
import Utils
import Conf
import Logger

import 			Control.Applicative((<|>)) 		-- for <|> operator
import 			Control.Monad

import 			Data.Attoparsec
import 			Data.Attoparsec.Combinator
import 			Data.Attoparsec.Char8 as P

import 			Text.ProtocolBuffers.Basic		-- for uFromString
import qualified	Text.ProtocolBuffers.WireMessage as Protobuf

import qualified 	Data.ByteString as BS
import qualified 	Data.ByteString.Char8 as BSC8
import 			Data.Word
import 			Data.Char
import 			Utils(maybeRead)

import System.ZMQ as ZMQ

parseRMC :: BS.ByteString -> Either String RMC
parseRMC input = processResult $ parse rmcParser input
    where processResult     (Fail a b c) 	= Left $ concat ["parseRMC fail with:'", (show c), (show b), (show a),"'"]
          processResult     (Partial fn) 	= processResult $ fn BS.empty
          processResult res@(Done _ r)		= chsmCheck r 

          chsmCheck (rmc, chsm) | checkRMCSum input chsm = 
                                    Right rmc
          chsmCheck (rmc, chsm) | otherwise 		 = 
                                    Left $ "parseRMC fail: CheckSum error:'" ++ show rmc ++ 
                                           ";\ncheckSum:" ++ show chsm ++ "'"

{- every field may be empty(except "$GPRMC,", \13 and checkSum) but must be correct
   mode indicator with (or without) comma may be omitted -}
rmcParser :: Parser (RMC, Int)
rmcParser = do preludeString "$GPRMC,"
               time		<- timeParser
               status		<- commaThen (many $ oneOf "AV")

               -- todo
               (lati, longi) 	<- commaThen locationParser

               speed		<- commaThen float
               sDirection	<- commaThen float
               date		<- commaThen dateParser
               mDecl		<- commaThen float
	       mDeclDir		<- commaThen (many $ oneOf "EW")
               m 		<- do { P.try $ preludeString ",*"; 	return Nothing}
                                      <|> do { char ','
                                             ; m' <- oneOf "ADEN"
                                             ; char '*'
                                             ; return $ Just [m']}
                                      <|> do { char '*'; 	return Nothing} 
               checkSum		<- (2 `count` hex)
               -- EOl must be at the end
               do P.try $ do {cr; lf}	
                  <|> cr
               return $ (RMC time (maybeRead status) lati longi (maybeRead speed)
                         (maybeRead sDirection) date (maybeRead mDecl) (maybeRead mDeclDir) (m >>= maybeRead),
                         readHex checkSum)
                   where cr = char $ chr 0x0d
                         lf = char $ chr 0x0a

readHex :: [Char] -> Int
readHex = read . ("0x" ++ )

preludeString = string . BSC8.pack

hex = digit <|> oneOf ['A' .. 'F'] <|> oneOf ['a' .. 'v'] 

commaThen p = char ',' >> p

-- FIXME must much [0-9]{0,}\.[0-9]{0,}
float :: Parser String
float = do { c  <- P.try (digit <|> char '.')
           ; cs <- float
           ; return $ c:cs}
             <|> return ""
-- float = many $ digit <|> char '.'

oneOf :: String -> Parser Char
oneOf [x]	= char x
oneOf (x:xs) 	= char x <|> oneOf xs
oneOf _		= error "oneOf: empty list"

----------------------------------------
timeParser :: Parser (Maybe Double)
timeParser = do { hours 		<- 2 `count` digit
                ; minutes		<- 2 `count` digit
                ; seconds		<- float
                ; return $ Just $ mkTime (read hours) (read minutes) (read seconds)
                } <|> return Nothing
    where mkTime h m s = s + 60*m + 3600*h

locationParser :: Parser (Maybe Double, Maybe Double)
locationParser = do latD	<- do 2 `count` digit
                                            <|> return ""
                    latM	<- float
                    p		<- commaThen (many $ oneOf "NS")
                    lonD	<- commaThen (do 3 `count` digit
                                                       <|> return "")
                    lonM	<- float
                    j		<- commaThen (many $ oneOf "EW")
                    return $ (mkLat latD latM p,
                              mkLon lonD lonM j)
    where mkLat latD latM p | stringsIsNotEmpty [latD, latM, p] = Just $ ((read latD)*60 + (read latM)) *
                                                                         if p == "N" then 1 else -1
                            | otherwise				= Nothing
          mkLon lonD lonM j | stringsIsNotEmpty [lonD, lonM, j] = Just $ ((read lonD)*60 + (read lonM)) *
                                                                         if j == "E" then 1 else -1
                            | otherwise				= Nothing

stringsIsNotEmpty :: [[Char]] -> Bool
stringsIsNotEmpty = (/= 0) . product . map length

dateParser :: Parser (Maybe Int32)
dateParser = do { day	<- 2 `count` digit
                ; month	<- 2 `count` digit
                ; year	<- 2 `count` digit	-- 2 not 4 digit, + 2000 years
                ; return $ Just $ mkDate (read day) (read month) (read year)
                } <|> return Nothing
    where mkDate d m y = d + m*31 + (2000+y)*31*12

--------------------------------------------------------------------------------
checkRMCSum :: BS.ByteString -> Int -> Bool
checkRMCSum input checkSum = calcRMCSum input == checkSum

--------------------------------------------------------------------------------

main :: [String] -> IO ()
main args = do 
  Doc.lengthArgsAssert (length args > 0)
  Doc.helpInArgsCheck args Doc.parserUsage
               
  let toBindN = args !! 0

  Doc.naturalNumAssert toBindN "argument must be an integer"

  nParser <- Conf.getNParser (read toBindN)

  let toBindI = nodeInput  nParser
      toBindO = nodeOutput nParser
               
  context <- ZMQ.init 1

  -- binding input socket
  iSock <- socket context Pull
  bind iSock toBindI

  oSock <- socket context Pub
  bind oSock toBindO

  putStrLn $ "running parser on |" ++ toBindI ++ " -> [] -> " ++
             toBindO ++ "| ..."
  -- FIXME not forever
  forever $ do
    rmcString <- receive iSock []
    case parseRMC rmcString of
      (Left  err) -> Logger.rmcParseError (BSC8.unpack rmcString) err
      (Right rmc) -> do send oSock (Utils.fromLazyBS $ Protobuf.messagePut rmc) []
                           
  return ()

  ZMQ.close iSock
  ZMQ.term context
  -- threadDelay 10000000
  return ()


{-
send ::
  Socket a -> Data.ByteString.Internal.ByteString -> [Flag] -> IO ()

Protobuf.messagePut ::
  (Text.ProtocolBuffers.Reflections.ReflectDescriptor msg,
   Wire msg) =>
  msg -> ByteString
  	-- Defined in Text.ProtocolBuffers.WireMessage
-}


--------------------------------------------------------------------------------
-- test = parseRMC . BSC8.pack
-- Examples:

-- parseRMC . BSC8.pack $ "$GPRMC,125504.049,A,5542.2389,N,03741.6063,E,0.06,25.82,200906,,*3B\13\10"
