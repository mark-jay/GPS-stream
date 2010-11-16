module NMEA.RMC where

import RMCProtobuf.RMC.RMC
import RMCProtobuf.RMC.RMCLocation
import RMCProtobuf.RMC.RMCTime
import RMCProtobuf.RMC.RMCDate
import RMCProtobuf.RMC.Angle
-- TODO

import 			Control.Applicative((<|>)) 		-- for <|> operator

import 			Data.Attoparsec
import 			Data.Attoparsec.Combinator
import 			Data.Attoparsec.Char8 as P

import 			Text.ProtocolBuffers.Basic		-- for uFromString

import qualified 	Data.ByteString as BS
import qualified 	Data.ByteString.Char8 as BSC8
import 			Data.Word
import 			Data.Char
import 			Network.CGI.Protocol(maybeRead)
import 			Data.Bits(xor)

parseRMC :: BS.ByteString -> Result (RMC, Int)
parseRMC = processResult . parse rmcParser
    where processResult     (Fail a b c) 	= error $ concat ["Fail with", (show a), (show b), (show c)]
          processResult     (Partial fn) 	= processResult $ fn BS.empty
          processResult res@(Done _ _)		= res

-- every field may be empty(except "$GPRMC,", \13 and checkSum) but must be correct
-- mode indicator with (or without) comma may be missed

rmcParser :: Parser (RMC, Int)
rmcParser = do preludeString "$GPRMC,"
               time		<- timeParser
               status		<- commaThen (many $ oneOf "AV")
               location 	<- commaThen locationParser
               speed		<- commaThen float
               sDirection	<- commaThen float
               date		<- commaThen dateParser
               mDecl		<- commaThen float
	       mDeclDir		<- commaThen (many $ oneOf "EW")
               m 		<- do { P.try $ preludeString ",*"; 	return Nothing}
                                      <|> do { char ','
                                             ; m' <- oneOf "ADEN"
                                             ; char '*'
                                             ; return $ Just $ uFromString [m']}
                                      <|> do { char '*'; 	return Nothing}
               checkSum		<- 2 `count` hex
               -- EOl must be at the end
               do P.try $ do {cr; lf}	
                  <|> cr
               return $ (RMC time (maybeRead status) location (maybeRead speed)
                         (maybeRead sDirection) date (maybeRead mDecl) (maybeRead mDeclDir) m,
                         readHex checkSum)
                   where cr = char $ chr 0x0d
                         lf = char $ chr 0x0a

readHex :: [Char] -> Int
readHex = read . ("0x" ++ )

preludeString = string . BSC8.pack

hex = oneOf ['A' .. 'F'] <|> oneOf ['a' .. 'v'] <|> digit

commaThen p = char ',' >> p

-- float = many $ digit <|> char '.'
float :: Parser String
float = do { c  <- P.try (digit <|> char '.')
           ; cs <- float
           ; return $ c:cs}
             <|> return ""

oneOf :: String -> Parser Char
oneOf [x]	= char x
oneOf (x:xs) 	= char x <|> oneOf xs
oneOf _		= error "oneOf: empty list"
----------------------------------------


timeParser :: Parser (Maybe RMCTime)
timeParser = do { hours 		<- 2 `count` digit
                ; minutes		<- 2 `count` digit
                ; seconds		<- float
                ; return $ Just $ RMCTime (read hours) (read minutes) (read seconds)
                } <|> return Nothing

locationParser :: Parser RMCLocation
locationParser = do latD	<- do 2 `count` digit
                                            <|> return ""
                    latM	<- float
                    p		<- commaThen (many $ oneOf "NS")
                    lonD	<- commaThen (do 3 `count` digit
                                                       <|> return "")
                    lonM	<- float
                    j		<- commaThen (many $ oneOf "EW")
                    return $ RMCLocation (mkAngle latD latM)
                                         (maybeRead p)
                                         (mkAngle lonD lonM)
                                         (maybeRead j)
    where mkAngle a b = if (a == "" &&
                            b == "")
                        then Nothing
                        else Just $ Angle (read a) (read b)

-- dateParser = undefined

dateParser :: Parser (Maybe RMCDate)
dateParser = do { day	<- 2 `count` digit
                ; month	<- 2 `count` digit
                ; year	<- 2 `count` digit
                ; return $ mkDate day month year
                } <|> return Nothing
    where mkDate a b c = if (a == "" &&
                             b == "" &&
                             c == "")
                         then Nothing
                         else Just $ RMCDate (read a) (read b) (read c)
                        

--------------------------------------------------------------------------------
checkRMCSum :: String -> Int -> Bool
checkRMCSum input checkSum = xorBits (f input) == checkSum
    where xorBits = foldl xor 0 . map ord
          -- select part we need
          f = Prelude.takeWhile (/= '*') . tail . dropWhile (/= '$')

--------------------------------------------------------------------------------

-- test = parseRMC . BSC8.pack

-- test  "$GPRMC,125504.049,,5542.2389,N,03741.6063,E,0.06,25.82,200906,,E*3B\13"

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- parseTest rmcParser "$GPRMC,125504.049,A,5542.2389,N,03741.6063,E,0.06,25.82,200906,,*3B\13"
-- parseTest locationParser "5542.2389,N,03741.6063,E"
-- 
-- parseRMC $ BSC8.pack  "$GPRMC,125504.049,,5542.2389,N,03741.6063,E,0.06,25.82,200906,,E*3B\13"
-- parseRMC $ BSC8.pack  "$GPRMC,125504.049,,5542.2389,N,03741.6063,E,0.06,25.82,,,E*3B\13"
-- parseRMC $ BSC8.pack  "$GPRMC,,V,,,,,,,080907,9.6,E,N*31\13"
-- parseRMC $ BSC8.pack  "$GPRMC,,,,,,,,,,,,N*31\13"
-- parseRMC $ BSC8.pack  "$GPRMC,,,,,,,,,,,,*31\13"
-- parseRMC $ BSC8.pack  "$GPRMC,,,,,,,,,,,E,E*31\13"
-- parseRMC $ BSC8.pack  "$GPRMC,,,,,,,,,,,E,*31\13"
-- parseRMC $ BSC8.pack  "$GPRMC,,,,,,,,,,,E*31\13"

-- checkRMCSum "$GPRMC,125504.049,A,5542.2389,N,03741.6063,E,0.06,25.82,200906,,*3B" 0x3b
-- checkRMCSum "$GPRMC,,V,,,,,,,080907,9.6,E,N*31" 0x31
