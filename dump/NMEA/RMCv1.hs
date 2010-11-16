module RMC where

{-
parseWith :: Show a => Parser a -> String -> 
parseWith p input
    = case (parse p "" input) of
        Left err -> error "incorrect parse: unimplemented yet"
        Right x -> x
-}

import Text.ParserCombinators.Parsec as P
import Data.Char
import Control.Exception
import Network.CGI.Protocol(maybeRead)

data RMC = RMC {
      time		:: RMCTime,
      status		:: Char,
      location		:: RMCLocation,
      speed		:: Maybe Double,
      sDirection	:: Maybe Double,	-- speed direction
      date		:: RMCDate,			
      mDecl		:: Maybe Double,	-- Magnetic declination
      mDeclDir		:: Char,		-- "e" or "w"
      modeIndicator	:: Maybe Char
    }deriving Show

--------------------------------------------------------------------------------

data RMCTime 		= RMCTime {
      rmcHours	 	:: Int,
      rmcMinutes	:: Int,
      rmcSeconds	:: Double
    }deriving Show
----------------------------------------
data RMCDate 		= RMCDate {
      day		:: Int,
      month		:: Int,
      year		:: Int
    }deriving Show
----------------------------------------
data RMCLocation 	= RMCLocation {
      latitude 		:: Angle,
      p			:: Char,
      longitude		:: Angle,
      j			:: Char
    }deriving Show

data Angle = Angle {
      degree	:: Int,
      minute	:: Double
    }deriving Show
----------------------------------------
parseRMC :: String -> Either ParseError (RMC, Int)
parseRMC = parse rmcParser ""

--             [time, status, latitude, p, longitude, j, speed, 
--              sDirection, date, mDecl, mDeclDir, indicator, checkSum] ->
rmcParser :: Parser (RMC, Int)
rmcParser = do string "$GPRMC,"
               time		<- timeParser
               char ','
               status		<- oneOf "AV"
               char ','
               location 	<- locationParser
               char ','
               speed		<- float
               char ','
               sDirection	<- float
               char ','
               date		<- dateParser
               char ','
               mDecl		<- float
               char ','
	       mDeclDir		<- oneOf "EW"
               m 		<- do { P.try $ char ',';
                                        m' <- oneOf "ADEN";
                                        char '*'; 
                                        return $ Just m'}
                                      <|> do { char '*'; return Nothing}
               checkSum		<- 2 `count` hex
               do P.try $ do {cr; lf}
                  <|> cr
               return $ (RMC time status location (maybeRead speed)
                         (maybeRead sDirection) date (maybeRead mDecl) mDeclDir m,
                         read ("0x" ++ checkSum))
                   where cr = char $ chr 0x0d
                         lf = char $ chr 0x0a

hex = oneOf ['A' .. 'F'] <|> oneOf ['a' .. 'v'] <|> digit

-- float = many $ digit <|> char '.'
float :: Parser String
float = do { c  <- P.try (digit <|> char '.')
           ; cs <- float
           ; return $ c:cs}
             <|> return ""
----------------------------------------

timeParser :: Parser RMCTime
timeParser = do hours 		<- 2 `count` digit
                minutes		<- 2 `count` digit
                seconds		<- float
                return $ RMCTime (read hours) (read minutes) (read seconds)

locationParser :: Parser RMCLocation
locationParser = do latD	<- 2 `count` digit
                    latM	<- float
                    char ','
                    p		<- oneOf "NS"
                    char ','
                    lonD	<- 3 `count` digit
                    lonM	<- float
                    char ','
                    j		<- oneOf "EW"
                    return $ RMCLocation (Angle (read latD) (read latM))
                                         p
                                         (Angle (read lonD) (read lonM))
                                         j

dateParser :: Parser RMCDate
dateParser = do day	<- 2 `count` digit
                month	<- 2 `count` digit
                year	<- 2 `count` digit
                return $ RMCDate (read day) (read month) (read year)
--------------------------------------------------------------------------------

test :: String -> IO()
test input = case parseRMC input of
               Left _ 			-> putStrLn "error"
               Right (rmc, checkSum)	-> do putStrLn "answer is:"
                                              print rmc
-- parseTest rmcParser "$GPRMC,125504.049,A,5542.2389,N,03741.6063,E,0.06,25.82,200906,,*3B"
-- parseTest locationParser "5542.2389,N,03741.6063,E"